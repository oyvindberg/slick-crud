package com.olvind.crud

import com.olvind.stringifiers.{RenderHint, Typename}

import scala.concurrent.Future

/**
 * Interface between client and server.
 *
 * We need this because client does not have access
 * to slick dependency, nor to cells with
 *  arbitrary conversion functions written by implementors.
 */

trait Editors {
  def editorDescs(): Seq[EditorDesc]
}

trait Editor {
  //emptyparens because of autowire
  def desc(): EditorDesc

  def cachedData    (user: UserInfo): Future[CachedData]
  def create        (user: UserInfo, params: Map[ColumnRef, StrValue]): Future[XRes[Option[StrRowId]]]
  def read          (user: UserInfo, paramsOpt: Option[QueryParams]): Future[XRes[Seq[StrTableRow]]]
  def readLinkedRows(user: UserInfo, idStr: StrRowId): Future[XRes[Seq[StrLinkedRows]]]
  def readRow       (user: UserInfo, idStr: StrRowId): Future[XRes[Option[StrTableRow]]]
  def update        (user: UserInfo, idStr: StrRowId, col: ColumnRef, value: StrValue): Future[XRes[(Option[StrValue], StrValue)]]
  def delete        (user: UserInfo, idStr: StrRowId): Future[XRes[Unit]]
}

case class UserInfo   (value: String) extends AnyVal
case class ColumnName (value: String) extends AnyVal
case class TableName  (value: String) extends AnyVal
case class EditorName (value: String) extends AnyVal
case class EditorId   (value: String) extends AnyVal

case class ColumnRef(
  table:     TableName,
  name:      ColumnName,
  isAutoInc: Boolean){
  override def toString = s"${table.value}(${name.value}})"
}

case class ColumnDesc(
  ref:        ColumnRef,
  typename:   Typename,
  rendering:  RenderHint,
  isOptional: Boolean,
  isEditable: Boolean){
  val name = ref.name
}

case class EditorDesc(
  editorName: EditorName,
  editorId:   EditorId,
  mainTable:  TableName,
  columns:    List[ColumnDesc],
  mainCols:   List[ColumnDesc],
  isEditable: Boolean) {
  def title = s"Editor ${editorName.value}"

  override def toString = s"EditorDesc(${mainTable.value})"
}


case class StrTableRow(
  idOpt:  Option[StrRowId],
  values: Seq[StrValue]
)

case class StrValue(value: String) extends AnyVal {
  def asId = StrRowId(value)
}
case class StrRowId(value: String) extends AnyVal {
  def asValue = StrValue(value)
}
case class StrLinkedRows(
  editorId:  EditorId,
  fromCol:   ColumnRef,
  toCol:     ColumnRef,
  rows:      Seq[StrTableRow]
)

case class CachedData(
  restrictedValues: RestrictedValues,
  tableLength:      TableLength){
  override val toString =
    s"CachedData(${restrictedValues.size}, $tableLength)"
}

case class TableLength(rows: Int) extends AnyVal