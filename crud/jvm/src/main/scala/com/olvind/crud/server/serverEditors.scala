package com.olvind.crud
package server

import slick.dbio.DBIO

import scala.concurrent.Future

trait serverEditors extends crudActions {

  final class ServerEditor[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P] private[server] (
                       val editorId:   EditorId,
                       val editorName: EditorName,
       private[server] val ref:        TableRef[ID, TABLE, LP, P]) extends Editor {

    //emptyparens because of autowire
    override def desc() = desc_

    private val desc_ = {
      def colsFrom[PP](m: Metadata[ID, PP]): List[ColumnDesc] =
        m.stringifiers.toList map {
          case (ci, stringifier) ⇒
            ColumnDesc(
              ci,
              stringifier.typename,
              stringifier.renderHint,
              stringifier.typename.value.startsWith("Option"),
              true /*stringifier.isEditable*/
            )
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

    def run[T](action: CrudDbOp[T]): Future[XRes[T]] =
      db.run(action.res) recover  {
        case th => XTechnicalMsg(th)
      }

    override def read(user: UserInfo, paramsOpt: Option[QueryParams]): Future[XRes[Seq[StrTableRow]]] =
      run(crudAction.readTable(ref, paramsOpt))

    override def create(user: UserInfo, params: Map[ColumnRef, StrValue]): Future[XRes[Option[StrRowId]]] =
      run(crudAction.create(ref.base, params))

    override def readRow(user: UserInfo, idStr: StrRowId): Future[XRes[Option[StrTableRow]]] =
      run(crudAction.readRow(ref, idStr))

    override def readLinkedRows(user: UserInfo, idStr: StrRowId): Future[XRes[Seq[StrLinkedRows]]] =
      run(crudAction.readLinked(ref, idStr))

    override def update(user: UserInfo, idStr: StrRowId, col: ColumnRef, value: StrValue): Future[XRes[(Option[StrValue], StrValue)]] =
      run(crudAction.update(ref, idStr, col, value))

    override def delete(user: UserInfo, idStr: StrRowId): Future[XRes[Unit]] =
      run(crudAction.delete(ref.base, idStr))

    override def cachedData(user: UserInfo): Future[CachedData] =
      db.run(cachedDataIO(user))

    private def cachedDataIO(user: UserInfo): DBIO[CachedData] =
      restrictedValuesIO zip tableLengthIO map CachedData.tupled

    private lazy val staticRestricted: RestrictedValues =
      ref.metadata.stringifiers.flatMap {
        case (columnRef, stringifier) ⇒
          stringifier.enumValuesOpt map (
            vs ⇒ (columnRef, (vs map (v => StrValue(stringifier encode v))).toSeq)
          )
      }.toMap

    private def restrictedValuesIO: DBIO[RestrictedValues] = {
      val queries = DBIO sequence (ref.linked map (_.restrictedValues))

      queries.map(results ⇒ staticRestricted ++ results.flatten.toMap)
    }

    private def tableLengthIO: DBIO[TableLength] =
      crudAction length ref.query
  }
}