package com.olvind.crud
package server

import slick.dbio.DBIO

import scala.concurrent.Future

trait serverEditors extends crudActions {

  case class ServerEditor[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
                         (ref: TableRef[ID, TABLE, LP, P],
                          n:   UpdateNotifier) extends Editor {
    val tableName = ref.metadata.tableName
    
    override val clientTable = {
      def colsFrom(m: Metadata[ID, _]): List[ClientColumn] =
        m.cells.toList map {
          case (ci, cell) ⇒
            ClientColumn(ci, cell.typeName, cell.rendering, cell.isOptional, cell.isEditable)
        }

      ClientTable(
        tableName,
        colsFrom(ref.metadata),
        colsFrom(ref.base.metadata),
        ref.base.isEditable
      )
    }

    private [serverEditors] case class run(user: UserInfo) {

      def handleBad(f: CrudFailure) =
        (f <| n.notifyFailure(user)) |> (_.left)

      def noFailure[T, S](action: DBIO[T])(handler: T ⇒ CrudFailure \/ S): Future[CrudFailure \/ S] =
        db run action map handler recoverWith {
          case t ⇒
            Future.successful(
              handleBad(
                CrudException(ref.metadata.tableName, ErrorMsg(t), "Unexpected Error")
              )
            )
        }

      def apply[T, F, S <: CrudSuccess]
               (actionE:    F \/ DBIO[F \/ T],
                handleGood: T ⇒ S,
                mapError:   F ⇒ CrudFailure): Future[CrudFailure \/ S] =
        actionE match {
          case \/-(action) ⇒
            noFailure(action){
              case -\/ (error) ⇒ handleBad(mapError(error))
              case  \/-(good)  ⇒ handleGood(good) <| n.notifySuccess(user) |> (_.right)
            }
          case -\/(error) ⇒ Future.successful(handleBad(mapError(error)))
        }
    }

    private implicit def promote1[F, T](t: F \/ DBIO[T]): F \/ DBIO[Nothing \/ T] = t.map(_.map(_.right))

    override def read(user: UserInfo, paramsOpt: Option[QueryParams]) =
      run(user).noFailure(crudAction.readTable(ref, paramsOpt)){
        rows ⇒ ReadTable(tableName, rows).right
      }

    override def create(user: UserInfo, params: Map[ColumnInfo, StrValue]) =
      run(user)[Option[StrRowId], (ErrorMsg \/ Seq[(ColumnInfo, ErrorMsg)]), Created](
        crudAction.create(ref.base, params),
        idOpt  ⇒ Created(tableName, idOpt),
        errors ⇒ CreateFailed(tableName, errors)
      )

    override def readRow(user: UserInfo, idStr: StrRowId) =
      run(user)[StrTableRow, ErrorMsg, ReadRow](
        crudAction.readRow(ref, idStr),
        row   ⇒ ReadRow(tableName, row),
        error ⇒ ReadRowFailed(tableName, idStr, error)
      )

    override def readLinkedRows(user: UserInfo, idStr: StrRowId) =
      run(user)[Seq[StrLinkedRows], ErrorMsg, ReadLinkedRows](
        crudAction.readLinked(ref, idStr),
        rows  ⇒ ReadLinkedRows(tableName, idStr, rows),
        error ⇒ ReadRowFailed(tableName, idStr, error)
      )

    override def update(user: UserInfo, idStr: StrRowId, col: ColumnInfo, value: StrValue) =
      run(user)[(Option[StrValue], StrValue), ErrorMsg, Updated](
        crudAction.update(ref, idStr, col, value),
        {case (from, to) ⇒ Updated(col, idStr, from, to)},
        error            ⇒ UpdateFailed(col, idStr, value, error)
      )


    override def delete(user: UserInfo, idStr: StrRowId) =
      run(user)[Unit, ErrorMsg, Deleted](
        crudAction.delete(ref.base, idStr),
        unit   ⇒ Deleted(tableName, idStr),
        error  ⇒ DeleteFailed(tableName, idStr, error)
      )

    override def cachedData(user: UserInfo) =
      run(user).noFailure(cachedDataIO(user))(cd ⇒ ReadCachedData(tableName, cd).right)

    private def cachedDataIO(user: UserInfo): DBIO[CachedData] =
      restrictedValuesIO zip tableLength map CachedData.tupled

    private def restrictedValuesIO: DBIO[RestrictedValues] = {
      val static: RestrictedValues =
        ref.metadata.cells.flatMap {
          case (ci, cell) ⇒
            cell.restrictedValues.map(vs ⇒ (ci, vs.map(cell.encode)))
        }.toMap

      val queries = DBIO sequence (ref.linked map (_.restrictedValues))

      queries.map {
        (results: List[Option[(ColumnInfo, Seq[StrValue])]]) ⇒
          static ++ results.flatten.toMap
      }
    }

    private def tableLength: DBIO[TableLength] =
      crudAction length ref.query
  }
}