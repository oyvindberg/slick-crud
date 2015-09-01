package com.olvind.crud
package server

import com.olvind.stringifiers.Stringifier
import slick.lifted.ColumnOrdered

import scala.language.implicitConversions
import scala.util.{Failure, Success}

trait crudActions extends tableRefs with integrationDb with columnPickers with slickHacks with dbOps {
  import driver.api._

  private [server] object crudAction {
    import slickHacks._

    private val sortOrder: SortOrder ⇒ Rep[Any] ⇒ ColumnOrdered[Any] =
      o ⇒ if (o =:= Asc) _.asc else _.desc

    def length[E, U](q: Query[E, U, Seq]): DBIO[TableLength] = 
      q.size.result map TableLength

    def readTable[ID, TABLE <: AbstractTable[_], LP, P]
                 (ref:       TableRef[ID, TABLE, LP, P],
                  paramsOpt: Option[QueryParams]): CrudDbOp[Seq[StrTableRow]] = {

      val sortedQ: ref.Q ⇒ ref.Q =
        q ⇒ paramsOpt.flatMap(_.sorting).fold(q) {
          case (colName, order) ⇒
            q sortBy (ColumnPicker(ref.query, colName) _ andThen sortOrder(order))
        }

      val filteredQ: ref.Q ⇒ ref.Q =
        q ⇒ paramsOpt.flatMap(_.filter).fold(q) {
          f ⇒
            q filter {
              table ⇒
                val col = ColumnPicker(ref.query, f.columnInfo)(table)
                val valueLower = f.value.value.toLowerCase
                col.asColumnOf[String].toLowerCase.indexOf(valueLower) =!= -1
            }
        }

      val pagedQ: ref.Q ⇒ ref.Q =
        q ⇒ paramsOpt.fold(q)(
          view ⇒ q drop view.start take view.pageSize
        )

      val q = (filteredQ andThen sortedQ andThen pagedQ)(ref.query)

      CrudDbOp fromDbio (q.result map (_ map (ref.metadata encodeRow None)))
    }

    def readRow[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
               (ref:   TableRef[ID, TABLE, LP, P],
                idStr: StrRowId): CrudDbOp[Option[StrTableRow]] =
      
      for {
        id   ← decodeId(ref, idStr)
        pOpt ← CrudDbOp fromDbio (ref queryById id).result.headOption
      } yield pOpt map (ref.metadata encodeRow id.some)

    def readLinked[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
                  (ref:   TableRef[ID, TABLE, LP, P],
                   idStr: StrRowId): CrudDbOp[Seq[StrLinkedRows]] =
      for {
        id       ← decodeId(ref, idStr)
        rows     ← CrudDbOp fromDbio (DBIO sequence (ref.linked map (_ linkedRows id)))
      } yield rows

    def update[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
              (ref:      TableRef[ID, TABLE, LP, P],
               idStr:    StrRowId,
               col:      ColumnRef,
               valueStr: StrValue): CrudDbOp[(Option[StrValue], StrValue)] =

      for {
        id          ← decodeId(ref, idStr)
        stringifier ← CrudDbOp fromOpt (ref.metadata stringifierFor col, XTechnicalMsg(s"table has no column ${col.name.value}"))
        _           ← CrudDbOp require (ref.base.isEditable, XTechnicalMsg("Table is not editable"))
        validValue  ← decode(idStr, col, stringifier, valueStr)
        rowQ        = ref.base queryById id
        updateQ     = rowQ map (ColumnPicker(rowQ, col) _ andThen ensureOptionalColumn(validValue))
        oldValueOpt ← CrudDbOp fromDbio updateQ.result.headOption
        n           ← CrudDbOp fromDbio (updateQ update validValue)
        _           ← ensureOneRowChanged(n)
      } yield (oldValueOpt map stringifier.encode map StrValue, StrValue(stringifier encode validValue))

    def create[ID: FlatRepShape, TABLE <: AbstractTable[_]]
              (ref:    BaseTableRef[ID, TABLE],
               strRow: Map[ColumnRef, StrValue]): CrudDbOp[Option[StrRowId]] = {

      def createQ(row: TABLE#TableElementType): DBIO[Option[ID]] = {
        val insertAutoGen: DBIO[ID] =
          ref.query returning (ref.query map ref.idCol) += row

        val insert: DBIO[Int] =
          ref.query += row

        insertAutoGen.asTry flatMap {
          case Success(id) ⇒ DBIO successful id.some
          case Failure(_)  ⇒ insert map (n ⇒ ref.metadata extractIdFromRow row)
        }
      }

      for {
        _     ← CrudDbOp require (ref.isEditable, XTechnicalMsg("Table is not editable"))
        row   ← CrudDbOp(ref.metadata.decodeRow(None, strRow))
        idOpt ← CrudDbOp fromDbio createQ(row)
      } yield idOpt map ref.metadata.encodeId
    }

    def delete[ID: FlatRepShape, TABLE <: AbstractTable[_]]
              (ref:   BaseTableRef[ID, TABLE],
               idStr: StrRowId): CrudDbOp[Unit] =
      for {
        id ← decodeId(ref, idStr)
        _  ← CrudDbOp require (ref.base.isEditable, XTechnicalMsg("Table is not editable"))
        _  ← CrudDbOp require (ref.canDelete, XTechnicalMsg("Can not delete from table"))
        n  ← CrudDbOp fromDbio (ref queryById id).delete
        _  ← ensureOneRowChanged(n)
      } yield ()


    private def ensureOneRowChanged(n: Int): CrudDbOp[Unit] = n match {
      case 1   ⇒ CrudDbOp success (())
      case 0   ⇒ CrudDbOp failure XTechnicalMsg("No rows matched")
      case num ⇒ CrudDbOp failure XTechnicalMsg(s"Rolled back because matched $num rows")
    }

    private def decode[T](strRowId: StrRowId, columnRef: ColumnRef, S: Stringifier[T], v: StrValue): CrudDbOp[T] =
      CrudDbOp[T](
        S decode v.value match {
          case Left(fail) => XValidation(Some(strRowId), Seq((columnRef, Some(fail))))
          case Right(id)  => XSuccess(id)
        }
      )

    private def decodeId[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P], idStr: StrRowId): CrudDbOp[ID] =
      decode(idStr, ref.metadata.idCol, ref.metadata.idStringifier, idStr.asValue)
  }
}