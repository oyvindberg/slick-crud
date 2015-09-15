package com.olvind.crud
package server

import slick.lifted.ColumnOrdered

import scala.language.implicitConversions
import scala.util.{Failure, Success}

trait crudActions extends tableRefs with dbIntegration with columnPickers with slickHacks with executionContexts {
  import driver.api._

  private [server] object crudAction {
    import slickHacks._

    type ErrorOrDb[T] = ErrorMsg \/ DBIO[T]

    private def panicAction(s: String): DBIO[Nothing] =
      DBIO failed new RuntimeException(s)

    private val sortOrder: SortOrder ⇒ Rep[Any] ⇒ ColumnOrdered[Any] =
      o ⇒ if (o =:= Asc) _.asc else _.desc

    def length[E, U](q: Query[E, U, Seq]): DBIO[TableLength] = 
      q.size.result map TableLength

    def readTable[ID, TABLE <: AbstractTable[_], LP, P]
                 (ref:       TableRef[ID, TABLE, LP, P],
                  paramsOpt: Option[QueryParams]): DBIO[Seq[StrTableRow]] = {

      val sortedQ: ref.Q ⇒ ref.Q =
        q ⇒ paramsOpt.flatMap(_.sorting).fold(q) {
          case (colName, order) ⇒
            q sortBy (ColumnPicker(ref.query, colName) _ andThen sortOrder(order))
        }

      val filteredQ: ref.Q ⇒ ref.Q =
        q ⇒ paramsOpt.flatMap(_.filter).fold(q) {
          case f ⇒
            q filter {
              table ⇒
                val col        = ColumnPicker(ref.query, f.columnInfo)(table)
                val valueLower = f.value.value.toLowerCase
                col.asColumnOf[String].toLowerCase.indexOf(valueLower) =!= -1
            }
        }

      val pagedQ: ref.Q ⇒ ref.Q =
        q ⇒ paramsOpt.fold(q)(
          view ⇒ q
            drop view.page.value * view.pageSize
            take view.pageSize
        )

      val q = (filteredQ andThen sortedQ andThen pagedQ)(ref.query)

      q.result map (_ map ref.metadata.encodeRow(None))
    }

    def readRow[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
               (ref:   TableRef[ID, TABLE, LP, P],
                idStr: StrRowId): ErrorOrDb[Option[StrTableRow]] =
      
      for {
        id ← ref.metadata decodeId idStr
      } yield (ref queryById id).result.headOption map {
        _ map (ref.metadata encodeRow id.some)
      }

    def readLinked[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
                  (ref:   TableRef[ID, TABLE, LP, P],
                   idStr: StrRowId): ErrorOrDb[Seq[StrLinkedRows]] =
      for {
        id       ← ref.metadata decodeId idStr
        linkedQs = ref.linked.map(_.linkedRows(id))
      } yield DBIO sequence linkedQs

    /**
     * Update a column 'columnName' for row with id 'id' with value 'value'
     *
     * We take two sets of named cells here. 'namedCellsQuery' just to verify that the
     *  columns to be updated are exposed by the current query, and we use 'namedCellsTable'
     *  for the actual update
     *
     *  @return old value of cell on success, error otherwise
     */
    def update[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
              (ref:      TableRef[ID, TABLE, LP, P],
               idStr:    StrRowId,
               col:      ColumnRef,
               valueStr: StrValue): ErrorOrDb[(Option[StrValue], StrValue)] =
      for {
        id         ← ref.metadata decodeId idStr
        cell       ← (ref.metadata lookupCellFor col) \/> ErrorMsg(s"table has no cell with name ${col.name.value}")
        validValue ← cell decode valueStr
        _          ← if (ref.base.isEditable) ().right else ErrorMsg("Table is not editable").left
      } yield {
        val rowQ    = ref.base queryById id
        val updateQ = rowQ map (ColumnPicker(rowQ, col) _ andThen ensureOptionalColumn(validValue))

        val action = for {
          oldValueOpt ← updateQ.result.headOption
          n           ← updateQ update validValue
          _           ← ensureOneRowChanged(n)
        } yield (oldValueOpt map cell.encode, cell encode validValue)

        action.transactionally
      }

    def create[ID: FlatRepShape, TABLE <: AbstractTable[_]]
              (ref:    BaseTableRef[ID, TABLE],
               strRow: Map[ColumnRef, StrValue]): (ErrorMsg \/ Seq[(ColumnRef, ErrorMsg)]) \/ DBIO[Option[StrRowId]] = {
      for {
        row ← (ref.metadata decodeRow strRow).leftMap(_.right)
        _   ← if (ref.isEditable) ().right else ErrorMsg("Table is not editable").left.left
      } yield {
        val insertAutoGen: DBIO[ID] =
          ref.query returning (ref.query map ref.idCol) += row

        val insert: DBIO[Int] =
          ref.query += row

        insertAutoGen.asTry flatMap {
          case Success(id) ⇒ DBIO successful id.some
          case Failure(_)  ⇒ insert map (n ⇒ ref.metadata.extractIdFromRow(row))
        } map (_ map ref.metadata.encodeId)
      }
    }

    def delete[ID, TABLE <: AbstractTable[_]]
              (ref:   BaseTableRef[ID, TABLE],
               idStr: StrRowId): ErrorOrDb[Unit] =
      for {
        id ← ref.metadata decodeId idStr
        _  ← if (ref.base.isEditable) ().right else ErrorMsg("Table is not editable").left
        _  ← if (ref.canDelete)       ().right else ErrorMsg("Can not delete from table").left
      } yield
        for {
          n ← (ref queryById id).delete
          _ ← ensureOneRowChanged(n)
        } yield ()

    private def ensureOneRowChanged(n: Int): DBIO[Unit] = n match {
      case 1   ⇒ DBIO.successful(())
      case 0   ⇒ panicAction("No rows matched")
      case num ⇒ panicAction(s"Rolled back because matched $num rows")
    }
  }
}