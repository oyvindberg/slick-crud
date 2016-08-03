package com.olvind.crud
package frontend

import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.RouterCtl

final case class Model private (
  routerCtl:     Option[RouterCtl[Route]],
  userInfo:      UserInfo,
  notifications: List[String],
  errorOpt:      Option[(String, Action)],
  cachedData:    Map[EditorId, CachedData],
  data:          Map[EditorId, EditorModel]){

  def editor(eid: EditorId): Option[EditorModel] =
    data.get(eid)

  def updatedEditor(eid: EditorId)(f: EditorModel ⇒ EditorModel): Model =
    copy(data = data.updated(eid, f(data(eid))))

  def currentEditorIdOpt(route: Route): Option[EditorId] =
    route match {
      case RouteEditorRow(eid, rowId) ⇒
        Some(eid)
      case RouteEditor(eid) ⇒
        Some(eid)
      case RouteCreateRow(eid) ⇒
        Some(eid)
      case RouteChooseEditor ⇒
        None
    }

  def currentRowIdOpt(route: Route): Option[StrRowId] =
    route match {
      case RouteEditorRow(eid, rowId) ⇒
        Some(rowId)
      case other ⇒
        None
    }
}

object Model {
  def init(userInfo: UserInfo, editors: Seq[EditorDesc]): Model =
    new Model(None, userInfo, Nil, None, Map.empty,
      editors.map(
        e ⇒ e.editorId → EditorModel(e, None, Map.empty, QueryParams.default, Map.empty)
      ).toMap
    )

  implicit val ReusableModel: Reusability[Model] =
    Reusability.byRef[Model]
}

final case class EditorModel(
  editor:           EditorDesc,
  rows:             Option[Seq[StrTableRow]],
  linkedRows:       Map[StrRowId, Seq[StrLinkedRows]],
  queryParams:      QueryParams,
  validationErrors: Map[Option[StrRowId], Map[ColumnRef, ValidationError]]
)

object EditorModel {
  implicit val ReusableEditorModel: Reusability[EditorModel] =
    Reusability.byRef[EditorModel]
}