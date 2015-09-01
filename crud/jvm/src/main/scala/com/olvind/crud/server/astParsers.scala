package com.olvind.crud
package server

import com.typesafe.scalalogging.LazyLogging
import slick.ast._
import slick.compiler.QueryCompiler
import slick.lifted._
import slick.util.Dumpable

trait astParsers extends integrationSlick {

  private [server] object AstParser extends LazyLogging {

    final class QueryParseException private[AstParser](msg: String) extends Exception(msg)

    /* use the query compiler to get a simpler AST to work with,
        in particular ResultSetMapping and Comprehensions.
       Here we add relationalPhases because it does
        some further rewrites we want */
    private val compiler = new QueryCompiler(
      QueryCompiler.standardPhases ++
      QueryCompiler.relationalPhases
    )

    private def compileAndIndex[E, U, C[_], T](q: Query[U, E, C])(f: (Symbol, IndexedResolver, Node) ⇒ T): T =
      (compiler run q.toNode).tree match {
        case tree@ResultSetMapping(gen, _, cols) ⇒
          val sql: String = {
            import driver.api._
            q.result.statements.head
          }

          f(gen, IndexedResolver(tree, sql), cols)
      }

    /* resolve all column names for the given query */
    def colNames[E, U, C[_]](q: Query[U, E, C]): Seq[ColumnRef] =
      compileAndIndex(q){
        case (_, resolver: IndexedResolver, cols: Node) ⇒
          resolver(cols)
      }

    def colName[E, U, C[_], T: FlatRepShape](q: Query[U, E, C])(col: U ⇒ Rep[T]): ColumnRef =
      colNames(q map col).head

    /* resolve the name of `rep` */
    def colName[E, U, C[_]](q: Query[U, E, C], rep: Rep[Any]): ColumnRef =
      compileAndIndex(q){
        case (gen: Symbol, resolver: IndexedResolver, _) ⇒
          /**
           * do some sneaky rewriting because `rep` doesn't belong to `q`,
           * that is the generated reference doesn't match
           */
          val repR = rep.toNode match {
            case OptionApply(s: Select) ⇒ OptionApply(s.copy(in = Ref(gen)))
            case s: Select              ⇒ s.copy(in = Ref(gen))
          }

          resolver(repR).head
      }

    private object IndexedResolver {
      def apply(n: Node, sql: String) = new IndexedResolver(buildIndex(n).toMap, sql)

      private def buildIndex(n: Node): Seq[(Symbol, Node)] = {
        val references: Seq[(Symbol, Node)] = n match {
          case n: Comprehension ⇒ n.from
          case n: DefNode       ⇒ n.nodeGenerators
          case else_            ⇒ Seq.empty
        }
        references ++ (n.nodeChildren flatMap buildIndex)
      }
    }

    private final class IndexedResolver(lookup: Map[Symbol, Node], sql: String) extends LazyLogging {

      def apply(cols: Node): Seq[ColumnRef] =
        resolveColumns(cols) <| (ret ⇒ logger debug withSql(s"Resolved $ret"))

      def withSql(s: String) = s"$s (for sql $sql)"

      /* any error in this piece of code is critical - so we just fail early */
      def panic(under: Option[Node], msg: String): Nothing =
        throw new QueryParseException(withSql(msg))

      @scala.annotation.tailrec
      def resolveColumns(cols: Node): Seq[ColumnRef] =
        cols match {
          case n: Select ⇒
            Seq(resolveOneColumn(n))
          case ProductNode(ch) ⇒
            ch map resolveOneColumn
          case n: UnaryNode ⇒
            resolveColumns(n.child)
        }

      def resolveOneColumn(c: Node): ColumnRef =
        innerResolve(c).toList match {
          case Nil ⇒
            panic(c.some, s"Couldn't resolve column $c")
          case head :: Nil ⇒
            logger debug s"Resolved column $head"
            head
          case tooMany ⇒
            panic(c.some, s"Resolved column $c to too many columns $tooMany")
        }

      def innerResolve(ref: Node): Seq[ColumnRef] = {
        logger trace s"current $ref"
        ref match {
          /* End state: This is where we find a columns' table and name */
          case Select(Ref(in), fs: FieldSymbol) ⇒
            Seq(toColumnInfo(in, fs))

          /* resolve column through reference */
          case Path(colRef :: (rest @ (_ :: _))) =>
            /* resolve the comprehension the column is contained in */
            val compRef = rest.reduceRight[Symbol] {
              case (ElementSymbol(idx), z) ⇒ lookup(z).asInstanceOf[DefNode].nodeGenerators(idx-1)._1
            }

            colRef match {
              case colRef: AnonSymbol ⇒
                lookup(compRef) match {
                  case c: Comprehension ⇒
                    val allCols: Seq[(Symbol, Node)] = c.select match {
                      case Some(StructNode(ch))          ⇒ ch
                      case Some(Pure(StructNode(ch), _)) ⇒ ch
                      case other                         ⇒
                        panic(c.some, s"Unexpected select $other for comprehension $c while looking up $colRef")
                    }
                    /* pick the column referenced by `colRef` */
                    val col = allCols collectFirst {
                      case (`colRef`, found) ⇒ found
                    } getOrElse
                      panic(c.some, s"Couldn't find $colRef among $allCols for comprehension $c")

                    /* resolve picked column */
                    innerResolve(col)
                }

              case ElementSymbol(idx) ⇒
                lookup(compRef) match {
                  case c: Comprehension ⇒
                    val allCols: Seq[Node] = c.select match {
                      case Some(ProductNode(ch))          ⇒ ch
                      case Some(Pure(ProductNode(ch), _)) ⇒ ch
                      case other                          ⇒
                        panic(c.some, s"Unexpected select $other for comprehension $c while looking up $colRef")
                    }
                    /* pick the column with the given (one-based) index */
                    val col: Node = allCols(idx - 1)
                    /* resolve picked column */
                    innerResolve(col)
                }
            }

          case a@Apply(nameSym, ch) ⇒
            nameSym match {
              case Library.SilentCast |
                   Library.Cast       |
                   Library.IfNull     ⇒
                ch flatMap innerResolve

              case other ⇒
                val rendered = (nameSym.name, ch.flatMap(innerResolve).map(_.name.value).toList) match {
                  case (fn, one :: two :: Nil) if fn.length == 1 ⇒
                    s"$one $fn $two"
                  case (fn, args) ⇒
                    s"$fn(${args.mkString(", ")})"
                }
                Seq(ColumnRef(TableName("<function>"), ColumnName(rendered), isAutoInc = false))
            }

          case LiteralNode(value) ⇒
            Seq(ColumnRef(TableName("<literal>"), ColumnName(value.toString), isAutoInc = false))

          case RebuildOption(_, data) ⇒
            innerResolve(data)

          case IfThenElse(clauses) ⇒
            val rs = clauses flatMap innerResolve
            val ns = rs map (_.name.value)
            ns.head match {
              case "Some(1) = Some(1)" ⇒ Seq(rs(1))
              case head ⇒
                val n = s"if ($head) then ${ns(1)}${ns.drop(2).headOption.fold("")(s ⇒ " else " + s)}"
                Seq(ColumnRef(TableName("<function>"), ColumnName(n), false))
            }

          case n: UnaryNode ⇒
            innerResolve(n.child)

          case other ⇒ Seq.empty
        }
      }

      def tableName(s: Symbol): TableName = {
        @scala.annotation.tailrec
        def go(n: Node): TableName =
          n match {
            case TableNode(_, name, _, _, _) =>
              TableName(name)
            case Comprehension(Seq((_, one)), _, _, _, _, _, _) =>
              go(one)
            case other: Dumpable =>
              panic(other.some, s"No table found for $s (found $other)")
          }

        lookup get s match {
          case Some(node) ⇒ go(node)
          case None       ⇒ panic(None, s"No node found for $s")
        }
      }

      def toColumnInfo(tableRef: Symbol, fs: FieldSymbol) =
        ColumnRef(
          tableName(tableRef),
          ColumnName(fs.name),
          fs.options contains ColumnOption.AutoInc
        )
    }
  }
}