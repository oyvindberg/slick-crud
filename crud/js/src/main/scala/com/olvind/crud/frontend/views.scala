package com.olvind.crud
package frontend

sealed trait View

case object ViewNone extends View

case class ViewOneLoading(
  editorId: EditorId,
  rowId:    StrRowId
) extends View

case class ViewOne(
  editor:           EditorDesc,
  rowId:            StrRowId,
  row:              StrTableRow,
  validationErrors: Map[ColumnRef, ValidationError],
  linkedRowsOpt:    Option[StrLinkedRows]
) extends View

case class ViewManyLoading(
  editorId:    EditorId,
  queryParams: QueryParams
) extends View

case class ViewMany(
  editor:           EditorDesc,
  queryParams:      QueryParams,
  rows:             Seq[StrTableRow],
  validationErrors: Map[StrRowId, Map[ColumnRef, ValidationError]]
) extends View

case class ViewCreate(
  editor:           EditorDesc,
  validationErrors: Map[ColumnRef, ValidationError]
) extends View