package com.olvind.crud
package server

import slick.dbio.DBIO

import scala.concurrent.Future

trait serverEditors extends crudActions {

  final class ServerEditor[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P] private[server] (
                     val editorId:   EditorId,
                     val editorName: EditorName,
     private[server] val ref:        TableRef[ID, TABLE, LP, P],
                         n:          UpdateNotifier
    ) extends Editor {

    //emptyparens because of autowire
    override def desc() = desc_

    private val desc_ = {
      def colsFrom[PP](m: Metadata[ID, PP]): List[ColumnDesc] =
        m.cells.toList map {
          case (ci, cell) ⇒
            ColumnDesc(ci, cell.typeName, cell.rendering, cell.isOptional, cell.isEditable)
        }

      EditorDesc(
        editorName = editorName,
        editorId   = editorId,
        mainTable  = ref.base.tableName,
        columns    = colsFrom(ref.metadata),
        mainCols   = colsFrom(ref.base.metadata),
        isEditable = ref.base.isEditable
      )
    }

    private def runDb[T](user: UserInfo)(action: DBIO[T]): Future[T] =
      (db run action) <| (_.onFailure {
        case t ⇒ n.notifyFailure(user)(CrudException(editorId, ErrorMsg(t), "Unexpected Error"))
      })

    private def runEitherDb[L, R](user: UserInfo)(actionE: L \/ DBIO[R]): Future[L \/ R] =
      liftEither(actionE.map(runDb(user)))

    private def liftEither[L, R](from: L \/ Future[R]): Future[L \/ R] =
      from match {
        case -\/(left)   => Future.successful(left.left)
        case  \/-(right) => right.map(_.right)
      }

    private def logE[L <: CrudFailure, R <: CrudSuccess](user: UserInfo)(f: Future[L \/ R]) =
      f <| (_.onSuccess{
        case -\/ (l) => n.notifyFailure(user)(l)
        case  \/-(r) => n.notifySuccess(user)(r)
      })

    private def log[R <: CrudSuccess](user: UserInfo)(f: Future[R]) =
      f <| (_.onSuccess{case r => n.notifySuccess(user)(r)})

    override def read(user: UserInfo, paramsOpt: Option[QueryParams]): Future[CrudFailure \/ ReadTable] =
      runDb(user)(crudAction.readTable(ref, paramsOpt)).map{
        case rows ⇒ ReadTable(editorId, rows).right
      } <| logE(user)

    override def create(user: UserInfo, params: Map[ColumnRef, StrValue]) =
      runEitherDb(user)(crudAction.create(ref.base, params)).map(_.bimap(
        errors ⇒ CreateFailed(editorId, errors),
        idOpt  ⇒ Created(editorId, idOpt)
      )) <| logE(user)

    override def readRow(user: UserInfo, idStr: StrRowId) =
      runEitherDb(user)(crudAction.readRow(ref, idStr)).map(_.bimap(
        error ⇒ ReadRowFailed(editorId, idStr, error),
        row   ⇒ ReadRow(editorId, idStr, row)
      )) <| logE(user)

    override def readLinkedRows(user: UserInfo, idStr: StrRowId) =
      runEitherDb(user)(crudAction.readLinked(ref, idStr)).map(_.bimap(
        error ⇒ ReadRowFailed(editorId, idStr, error),
        rows  ⇒ ReadLinkedRows(editorId, idStr, rows)
      )) <| logE(user)

    override def update(user: UserInfo, idStr: StrRowId, col: ColumnRef, value: StrValue) =
      runEitherDb(user)(crudAction.update(ref, idStr, col, value)).map(_.bimap(
        error => UpdateFailed(editorId, col, idStr, value, error),
        {case (from, to) => Updated(editorId, col, idStr, from, to)}
      )) <| logE(user)

    override def delete(user: UserInfo, idStr: StrRowId) =
      runEitherDb(user)(crudAction.delete(ref.base, idStr)).map(_.bimap(
        error  ⇒ DeleteFailed(editorId, idStr, error),
        unit   ⇒ Deleted(editorId, idStr)
      )) <| logE(user)

    override def cachedData(user: UserInfo) =
      runDb(user)(cachedDataIO(user)).map(cd ⇒ ReadCachedData(editorId, cd)) <| log(user)

    private def cachedDataIO(user: UserInfo): DBIO[CachedData] =
      restrictedValuesIO zip tableLengthIO map CachedData.tupled

    private def restrictedValuesIO: DBIO[RestrictedValues] = {
      val static: RestrictedValues =
        ref.metadata.cells.flatMap {
          case (ci, cell) ⇒
            cell.restrictedValues.map(vs ⇒ (ci, vs.map(cell.encode)))
        }.toMap

      val queries = DBIO sequence (ref.linked map (_.restrictedValues))

      queries.map {
        (results: List[Option[(ColumnRef, Seq[StrValue])]]) ⇒
          static ++ results.flatten.toMap
      }
    }

    private def tableLengthIO: DBIO[TableLength] =
      crudAction length ref.query
  }
}