package com.olvind.crud
package server

import slick.lifted.{Query, Rep}

trait columnPickers extends astParsers {

  /**
   * Given a `table`, extract the `Rep[_]` for column `colInfo`.
   *
   * Note that the result cannot be reused/cached, as `table`, and hence
   * the column we return, internally depends on the query in which it was generated
   */
  private [server] object ColumnPicker {
    class ColumnPickerException private[ColumnPicker](val message: String) extends Exception

    def apply[U, E](from: Query[U, E, Seq], colInfo: ColumnRef)(table: Any): Rep[Any] = {
      columnsExtract(table) find  {
        col ⇒  AstParser.colName(from, col) =:= colInfo
      } getOrElse (throw new ColumnPickerException(s"Column $colInfo not found"))
    }

    private val u      = scala.reflect.runtime.universe
    private val mirror = u.runtimeMirror(this.getClass.getClassLoader)

    private val columnsExtract: PartialFunction[Any, Iterable[Rep[Any]]] = {
      case slickTable: AbstractTable[_] ⇒
        /** To get at the columns here, we have to resort to reflection
          * - I don't see any other way */
        val reflected = mirror reflect slickTable

        def hasRepReturnType(s: u.SymbolApi) =
          s.typeSignature.resultType.erasure =:= u.typeOf[Rep[Any]].erasure

        reflected.symbol.asType.toType.members.collect {
          case m if hasRepReturnType(m) && m.isMethod ⇒
            reflected.reflectMethod(m.asMethod).apply().asInstanceOf[Rep[Any]]

          case m if hasRepReturnType(m) && !m.isTerm ⇒
            reflected.reflectField(m.asTerm).get.asInstanceOf[Rep[Any]]
        }

      case p: Product ⇒
        p.productIterator.map(_.asInstanceOf[Rep[Any]]).toList

      case r: Rep[_] ⇒ Seq(r.asInstanceOf[Rep[Any]])
    }
  }
}
