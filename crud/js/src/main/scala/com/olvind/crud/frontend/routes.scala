package com.olvind.crud
package frontend

sealed trait Route
case object RouteChooseEditor extends Route
case class  RouteEditor(t: EditorDesc) extends Route
case class  RouteCreateRow(t: EditorDesc) extends Route
case class  RouteEditorRow(t: EditorDesc, id: StrRowId) extends Route
