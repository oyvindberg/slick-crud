package com.olvind.crud
package frontend

sealed trait Route
case object RouteChooseEditor extends Route
case class  RouteEditor(t: ClientTable) extends Route
case class  RouteCreateRow(t: ClientTable) extends Route
case class  RouteEditorRow(t: ClientTable, id: StrRowId) extends Route
