package com.olvind.crud

sealed abstract class CrudResult(table: TableName, 
                                 oid:   Option[StrRowId], 
                                 oc:    Option[ColumnName]) {
  def desc: String
  
  final def formatted = {
    val t  = s"Table ${table.value}".some
    val c  = oc.map(c ⇒ s"column ${c.value}")
    val id = oid.map(id ⇒ s"id ${id.value}")
    List(t, c, id).flatten.mkString("", ", ", ": ") + desc
  }
}

sealed abstract class CrudSuccess(table: TableName, idStr: Option[StrRowId], oc: Option[ColumnName], val desc: String) extends CrudResult(table, idStr, oc)

sealed trait Read

case class Created(table: TableName, oid: Option[StrRowId])
  extends CrudSuccess(table, oid, None, s"Created row")

case class ReadCachedData(table: TableName, cd: CachedData)
  extends CrudSuccess(table, None, None, "Read cached data") with Read

case class ReadLinkedRows(table: TableName, id: StrRowId, linkedRows: Seq[StrLinkedRows])
  extends CrudSuccess(table, id.some, None, s"Read linked rows") with Read

case class ReadRow(table: TableName, row: StrTableRow)
  extends CrudSuccess(table, row.idOpt, None, s"Read row") with Read

case class ReadTable(table: TableName, rows: Seq[StrTableRow])
  extends CrudSuccess(table, None, None, s"Read ${rows.size} rows") with Read

case class Updated(col: ColumnInfo, id: StrRowId, oldValue: Option[StrValue], newValue: StrValue)
  extends CrudSuccess(col.table, id.some, col.name.some, s"Updated from ${oldValue.fold("empty")(f ⇒ s"«${f.value}»")} to «${newValue.value}»")

case class Deleted(table: TableName, id: StrRowId)
  extends CrudSuccess(table, id.some, None, s"Deleted row")


sealed abstract class CrudFailure(table: TableName, oid: Option[StrRowId], oc: Option[ColumnName], e: ErrorMsg, failDesc: String) extends CrudResult(table, oid, oc) {
  override final def desc = s"$failDesc: ${e.value}"
}

case class CreateFailed (table: TableName, ve: ErrorMsg \/ Seq[(ColumnInfo, ErrorMsg)])
  extends CrudFailure(table, None, None, ve.fold(identity, es ⇒ ErrorMsg(es.map{case (col, e) ⇒ s"$col: ${e.value}"}.mkString(", "))), "Failed to create new row")

case class ReadRowFailed(table: TableName, id: StrRowId, e: ErrorMsg)
  extends CrudFailure(table, id.some, None, e, "Failed to read row")

case class UpdateFailed (col: ColumnInfo, id: StrRowId, value: StrValue, e: ErrorMsg)
  extends CrudFailure(col.table, id.some, col.name.some, e, s"Failed to update row to value «${value.value}»")

case class DeleteFailed (table: TableName, id: StrRowId, e: ErrorMsg)
  extends CrudFailure(table, id.some, None, e, s"Failed to delete row")

case class CrudException(table: TableName, e: ErrorMsg, s: String)
  extends CrudFailure(table, None, None, e, s)

class UpdateNotifier {
  def notifySuccess(user: UserInfo)(s: CrudSuccess) = ()
  def notifyFailure(user: UserInfo)(s: CrudFailure) = ()
}

object CrudResult{
  import upickle.default._

  implicit val CrudSuccessR: Reader[CrudSuccess] = Reader[CrudSuccess](
    implicitly[Reader[Created       ]].read orElse
    implicitly[Reader[ReadCachedData]].read orElse
    implicitly[Reader[ReadLinkedRows]].read orElse
    implicitly[Reader[ReadRow       ]].read orElse
    implicitly[Reader[ReadTable     ]].read orElse
    implicitly[Reader[Updated       ]].read orElse
    implicitly[Reader[Deleted       ]].read
  )

  implicit val CrudFailureR: Reader[CrudFailure] = Reader[CrudFailure](
    implicitly[Reader[CreateFailed  ]].read orElse
    implicitly[Reader[ReadRowFailed ]].read orElse
    implicitly[Reader[UpdateFailed  ]].read orElse
    implicitly[Reader[DeleteFailed  ]].read orElse
    implicitly[Reader[CrudException ]].read
  )

  implicit val CrudResultR: Reader[CrudResult] = Reader[CrudResult](
    implicitly[Reader[CrudSuccess]].read orElse
    implicitly[Reader[CrudFailure]].read
  )

  implicit val CrudSuccessW: Writer[CrudSuccess] = Writer[CrudSuccess] {
    case x: Created        ⇒ writeJs(x)
    case x: ReadCachedData ⇒ writeJs(x)
    case x: ReadLinkedRows ⇒ writeJs(x)
    case x: ReadRow        ⇒ writeJs(x)
    case x: ReadTable      ⇒ writeJs(x)
    case x: Updated        ⇒ writeJs(x)
    case x: Deleted        ⇒ writeJs(x)
  }
  implicit val CrudFailureW: Writer[CrudFailure] = Writer[CrudFailure] {
    case x: CreateFailed  ⇒ writeJs(x)
    case x: ReadRowFailed ⇒ writeJs(x)
    case x: UpdateFailed  ⇒ writeJs(x)
    case x: DeleteFailed  ⇒ writeJs(x)
    case x: CrudException ⇒ writeJs(x)
  }
  implicit val CrudResultW: Writer[CrudResult] = Writer[CrudResult] {
    case x: CrudSuccess   ⇒ writeJs(x)
    case x: CrudFailure   ⇒ writeJs(x)
  }
}