package com.olvind.crud
package server

import java.util.UUID

import slick.lifted.CanBeQueryCondition

trait tableRefs extends tableMetadata with executionContexts {
  import driver.api._

  private[server] sealed trait TableRefU {
    def parents: Seq[TableRefU]
    def desc(on: Option[EditorName]): EditorDesc

    final val id = EditorId(UUID.randomUUID().toString)

    final lazy val parentRefs: Map[EditorId, TableRefU] = {
      def go(acc: Map[EditorId, TableRefU], ref: TableRefU): Map[EditorId, TableRefU] =
        if (acc contains ref.id) acc else //break cycle
          ref.parents.foldLeft(acc + (ref.id → ref))(go)

      go(Map.empty, this)
    }
  }
  
  /**
   *  A reference to a slick table

   *  The names of type parameters of this table abstraction are reused within
   *   the whole codebase. Sometimes you will find Q(L)P or O(L)P, in which case
   *   they refer to a query or reference to another table, respectively.
   *
   * @tparam ID the primary key column, for example Rep[Int]
   * @tparam TABLE row type for a database reference, ie. the class of the table definition
   * @tparam LP the lifted projection, for example (Rep[Int], Rep[String])
   * @tparam P the projection, for example (Int, String)
   */
  abstract class TableRef[ID, TABLE <: AbstractTable[_], LP, P] extends TableRefU {
    final type BaseQ = Query[TABLE, TABLE#TableElementType, Seq]
    final type     Q = Query[LP, P, Seq]

    def desc(on: Option[EditorName]) = ToTableDesc(this, on)

    val base:              BaseTableRef[ID, TABLE]
    def linked:            List[LinkedTable[ID]]
    val metadata:          Metadata[ID, P]
    val query:             Q
    def queryById(id: ID): Q
  }

  case class BaseTableRef[ID: BaseColumnType, P <: AbstractTable[_]]
                         (query:      TableQuery[P],
                          isEditable: Boolean,
                          canDelete:  Boolean,
                          idCol:      P ⇒ Rep[ID],
                          idCell:     Cell[ID],
                          cellRow:    CellRow[P#TableElementType]) extends TableRef[ID, P, P, P#TableElementType]{
    val tableName                  = TableName(query.baseTableRow.tableName)
    override val base              = this
    override val linked            = Nil
    override val metadata          = Metadata.base(tableName, query, idCol, idCell, cellRow)
    override def queryById(id: ID) = query filter (idCol andThen (_ === id))
    override val parents           = Nil
  }

  case class ProjectedTableRef[ID, TABLE <: AbstractTable[_], LP, P, OLP, OP]
                              (wrapped: TableRef[ID, TABLE, LP, P],
                               proj:    Query[LP, P, Seq] ⇒ Query[OLP, OP, Seq],
                               cellRow: CellRow[OP] ) extends TableRef[ID, TABLE, OLP, OP]{

    override val base              = wrapped.base
    override val query             = wrapped.query |> proj
    override def queryById(id: ID) = (wrapped queryById id) |> proj
    override val metadata          = Metadata.derive(query, wrapped.metadata, cellRow)
    override val linked            = wrapped.linked
    override val parents           = Seq(wrapped)
  }

  sealed trait LinkedTable[ID] {
    def linkedRows(id: ID): DBIO[StrLinkedRows]
    def restrictedValues: DBIO[Option[(ColumnRef, Seq[StrValue])]]
  }

  case class LinkingTableRef[ ID,  TABLE <: AbstractTable[_],  LP,  P,
                             OID, OTABLE <: AbstractTable[_], OLP, OP,
                               C: FlatRepShape: Cell, OC: FlatRepShape: Cell, R]
                            (from:    TableRef[ID, TABLE, LP, P],
                             fromCol: LP               ⇒ Rep[C],
                             toCol:   OLP              ⇒ Rep[OC],
                             pred:   (Rep[C], Rep[OC]) ⇒ Rep[R])
                            (_to:                      ⇒ TableRef[OID, OTABLE, OLP, OP])
                       (implicit ev: CanBeQueryCondition[Rep[R]]) extends TableRef[ID, TABLE, LP, P] {
    lazy val to = _to

    lazy val link: LinkedTable[ID] = new LinkedTable[ID]{
      val fromColumn = AstParser.colName(from.query)(fromCol)
      val   toColumn = AstParser.colName(to.query)(toCol)
      val         rq = to.query map toCol

      //todo: this should all rather live in crudAction,
      //but it's a lot of logisticts to get all the types there, so...

      override def linkedRows(id: ID) = {
        /* f(select OP from 'to' where fromCol(from) is toCol(to)) */
        val q: Query[(Rep[C], OLP), (C, OP), Seq] =
          (from queryById id map fromCol)
            .join(to.query).on((f, to) ⇒ pred(f, toCol(to)))

        q.result map {
          (rows: Seq[(C, OP)]) ⇒
            StrLinkedRows(
              to.desc(None),
              fromColumn,
              toColumn,
              rows map (_._2) map to.metadata.encodeRow(None)
            )
        }
      }

      override def restrictedValues =
        rq.size.result flatMap {
          case n if n < maxNumLinks ⇒ rq.result map (vs ⇒ (fromColumn, vs map Cell[OC].encode).some)
          case _                    ⇒ DBIO successful None
        }
    }

    override val base              = from.base
    override def linked            = from.linked :+ link
    override val metadata          = from.metadata
    override val query             = from.query
    override def queryById(id: ID) = from queryById id
    override lazy val parents      = Seq(from, to)
  }

  private[server] object ToTableDesc {
    def apply[ID, TABLE <: AbstractTable[_], LP, P]
             (r:  TableRef[ID, TABLE, LP, P],
              on: Option[EditorName]): EditorDesc =
      EditorDesc(
        editorName = on,
        editorId   = r.id,
        mainTable  = r.base.tableName,
        columns    = colsFrom(r.metadata),
        mainCols   = colsFrom(r.base.metadata),
        isEditable = r.base.isEditable
      )

    private def colsFrom[ID, P](m: Metadata[ID, P]): List[ColumnDesc] =
      m.cells.toList map {
        case (ci, cell) ⇒
          ColumnDesc(ci, cell.typeName, cell.rendering, cell.isOptional, cell.isEditable)
      }
  }
}
