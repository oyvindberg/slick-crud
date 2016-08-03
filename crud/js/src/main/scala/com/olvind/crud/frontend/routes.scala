package com.olvind.crud
package frontend

sealed trait Route
case object RouteChooseEditor extends Route
final case class  RouteEditor(t: EditorId) extends Route
final case class  RouteCreateRow(t: EditorId) extends Route
final case class  RouteEditorRow(t: EditorId, id: StrRowId) extends Route
