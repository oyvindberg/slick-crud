package com.olvind.crud

import scala.concurrent.Future

/**
 * Interface between client and server.
 *
 * We need this because client does not have access
 * to slick dependency, nor to cells with
 *  arbitrary conversion functions written by implementors.
 */

trait Editors {
  def tables(): List[ClientTable]
}

trait Editor {
  type Ret[T] = Future[CrudFailure \/ T]
  //emptyparens because of autowire
  def clientTable(): ClientTable
  def cachedData(user: UserInfo): Ret[ReadCachedData]

  def create        (user: UserInfo, params: Map[ColumnInfo, StrValue]): Ret[Created]
  def read          (user: UserInfo, paramsOpt: Option[QueryParams]): Ret[ReadTable]
  def readLinkedRows(user: UserInfo, idStr: StrRowId): Ret[ReadLinkedRows]
  def readRow       (user: UserInfo, idStr: StrRowId): Ret[ReadRow]
  def update        (user: UserInfo, idStr: StrRowId, col: ColumnInfo, value: StrValue): Ret[Updated]
  def delete        (user: UserInfo, idStr: StrRowId): Ret[Deleted]
}

case class UserInfo(value: String)   extends AnyVal
case class ColumnName(value: String) extends AnyVal
case class TableName(value: String)  extends AnyVal

case class ColumnInfo(
  table:     TableName,
  name:      ColumnName,
  isAutoInc: Boolean
){
  override def toString = s"${table.value}(${name.value}})"
}

case class ClientTable(
  name:            TableName,
  columns:         List[ClientColumn],
  originalColumns: List[ClientColumn],
  isEditable:      Boolean) {
  override def toString = s"ClientTable(${name.value})"
}

case class ClientColumn(
  column:     ColumnInfo,
  typeName:   String,
  rendering:  CellRendering,
  isOptional: Boolean,
  isEditable: Boolean){
  val name = column.name
}


case class StrTableRow(
  idOpt:  Option[StrRowId],
  values: Seq[StrValue]
)

case class StrValue(value: String)   extends AnyVal{
  def asId = StrRowId(value)
}
case class StrRowId(value: String)   extends AnyVal{
  def asValue = StrValue(value)
}
case class StrLinkedRows(
  fromCol:   ColumnInfo,
  toCol:     ColumnInfo,
  fromValue: Option[StrValue],
  rows:      Seq[StrTableRow]
)

case class CachedData(
  restrictedValues: RestrictedValues,
  tableLength:      TableLength){
  override val toString =
    s"CachedData(${restrictedValues.size}, $tableLength)"
}

case class TableLength(rows: Int)    extends AnyVal
