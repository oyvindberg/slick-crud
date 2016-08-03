package com.olvind.crud
package frontend

import diode.ActionType
import japgolly.scalajs.react.extra.router.RouterCtl

sealed trait Action

object Action {
  implicit object ActionEv extends ActionType[Action]
}

sealed trait ReadFurtherAction extends Action {def eid: EditorId}
final case class FetchFilteredData(eid: EditorId, filterOpt: Option[Filter]) extends ReadFurtherAction
final case class FetchSortedData(eid: EditorId, sorting: ColumnRef) extends ReadFurtherAction
final case class FetchMoreData(eid: EditorId) extends ReadFurtherAction

final case class FetchEditorData(eid: EditorId) extends Action
final case class FetchCachedData(eid: EditorId) extends Action
final case class FetchRow(eid: EditorId, row: StrRowId) extends Action
final case class FetchLinkedRows(eid: EditorId, row: StrRowId) extends Action
final case class RefreshEditor(eid: EditorId) extends Action
final case class UpdateValue(eid: EditorId, idStr: StrRowId, col: ColumnRef, value: StrValue) extends Action
final case class CreateRow(eid: EditorId, params: Map[ColumnRef, StrValue]) extends Action
final case class ClearValidationError(eid: EditorId, rowId: Option[StrRowId], columnRef: Option[ColumnRef]) extends Action
final case class DeleteRow(eid: EditorId, rowId: StrRowId) extends Action
final case class Navigate(route: Route) extends Action

sealed trait InternalAction
final case class ChangeModel(f: Model ⇒ Model) extends InternalAction
final case class SetRouterCtl(routerCtl: RouterCtl[Route]) extends InternalAction

object InternalAction {
  implicit object InternalActionEv extends ActionType[InternalAction]
}