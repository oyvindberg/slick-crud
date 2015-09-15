package com.olvind.crud
package server

import slick.dbio.DBIO

import scala.concurrent.Future

trait serverEditors extends crudActions {

  final case class ServerEditor
    [ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
    (editorName: EditorName,
     ref:        TableRef[ID, TABLE, LP, P],
     n:          UpdateNotifier
    ) extends ServerEditorT[ID, TABLE, LP, P](ref, n) {

    override lazy val desc = ref.desc(editorName.some)

    lazy val parentEditors: Map[EditorId, ParentServerEditor[ID, TABLE, _, _]] =
      ref.parentRefs.mapValues(
        r ⇒ new ParentServerEditor(r.asInstanceOf[TableRef[ID, TABLE, _, _]], n)
      )
  }

  private[serverEditors] final case class ParentServerEditor
    [ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
    (ref: TableRef[ID, TABLE, LP, P],
     n:   UpdateNotifier) extends ServerEditorT[ID, TABLE, LP, P](ref, n){

    override lazy val desc = ref.desc(None)
  }

  private[serverEditors] sealed abstract class ServerEditorT
    [ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
    (ref: TableRef[ID, TABLE, LP, P],
     n:   UpdateNotifier) extends Editor {

    def runDb[T](user: UserInfo)(action: DBIO[T]): Future[T] =
      (db run action) <| (_.onFailure {
        case t ⇒ n.notifyFailure(user)(CrudException(ref.id, ErrorMsg(t), "Unexpected Error"))
      })

    def runEitherDb[L, R](user: UserInfo)(actionE: L \/ DBIO[R]): Future[L \/ R] =
      liftEither(actionE.map(runDb(user)))

    def liftEither[L, R](from: L \/ Future[R]): Future[L \/ R] =
      from match {
        case -\/(left)   => Future.successful(left.left)
        case  \/-(right) => right.map(_.right)
      }

    def logE[L <: CrudFailure, R <: CrudSuccess](user: UserInfo)(f: Future[L \/ R]) = f <| (_.onSuccess{
      case -\/ (l) => n.notifyFailure(user)(l)
      case  \/-(r) => n.notifySuccess(user)(r)
    })

    def log[R <: CrudSuccess](user: UserInfo)(f: Future[R]) =
      f <| (_.onSuccess{case r => n.notifySuccess(user)(r)})

    override def read(user: UserInfo, paramsOpt: Option[QueryParams]): Future[CrudFailure \/ ReadTable] =
      runDb(user)(crudAction.readTable(ref, paramsOpt)).map{
        case rows ⇒ ReadTable(ref.id, rows).right
      } <| logE(user)

    override def create(user: UserInfo, params: Map[ColumnRef, StrValue]) =
      runEitherDb(user)(crudAction.create(ref.base, params)).map(_.bimap(
        errors ⇒ CreateFailed(ref.id, errors),
        idOpt  ⇒ Created(ref.id, idOpt)
      )) <| logE(user)

    override def readRow(user: UserInfo, idStr: StrRowId) =
      runEitherDb(user)(crudAction.readRow(ref, idStr)).map(_.bimap(
        error ⇒ ReadRowFailed(ref.id, idStr, error),
        row   ⇒ ReadRow(ref.id, idStr, row)
      )) <| logE(user)

    override def readLinkedRows(user: UserInfo, idStr: StrRowId) =
      runEitherDb(user)(crudAction.readLinked(ref, idStr)).map(_.bimap(
        error ⇒ ReadRowFailed(ref.id, idStr, error),
        rows  ⇒ ReadLinkedRows(ref.id, idStr, rows)
      )) <| logE(user)

    override def update(user: UserInfo, idStr: StrRowId, col: ColumnRef, value: StrValue) =
      runEitherDb(user)(crudAction.update(ref, idStr, col, value)).map(_.bimap(
        error => UpdateFailed(ref.id, col, idStr, value, error),
        {case (from, to) => Updated(ref.id, col, idStr, from, to)}
      )) <| logE(user)

    override def delete(user: UserInfo, idStr: StrRowId) =
      runEitherDb(user)(crudAction.delete(ref.base, idStr)).map(_.bimap(
        error  ⇒ DeleteFailed(ref.id, idStr, error),
        unit   ⇒ Deleted(ref.id, idStr)
      )) <| logE(user)

    override def cachedData(user: UserInfo) =
      runDb(user)(cachedDataIO(user)).map(cd ⇒ ReadCachedData(ref.id, cd)) <| log(user)

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
        (results: List[Option[(ColumnRef, Seq[StrValue])]]) ⇒
          static ++ results.flatten.toMap
      }
    }

    private def tableLength: DBIO[TableLength] =
      crudAction length ref.query
  }
}