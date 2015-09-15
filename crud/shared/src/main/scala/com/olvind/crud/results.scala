package com.olvind.crud

sealed abstract class CrudResult(editorId: EditorId,  
                                 oid:      Option[StrRowId],
                                 oc:       Option[ColumnRef]) {
  def desc: String
  
  final def formatted = {
    val t  = s"Editor ${editorId.value}".some
    val c  = oc.map(c ⇒ s"column ${c.name.value} in table ${c.table.value}")
    val id = oid.map(id ⇒ s"id ${id.value}")
    List(t, c, id).flatten.mkString("", ", ", ": ") + desc
  }
}

sealed abstract class CrudSuccess(editorId: EditorId, idStr: Option[StrRowId], oc: Option[ColumnRef], val desc: String) extends CrudResult(editorId, idStr, oc)

sealed trait Read

case class Created(editorId: EditorId, oid: Option[StrRowId])
  extends CrudSuccess(editorId, oid, None, s"Created row")

case class ReadCachedData(editorId: EditorId, cd: CachedData)
  extends CrudSuccess(editorId, None, None, "Read cached data") with Read

case class ReadLinkedRows(editorId: EditorId, id: StrRowId, linkedRows: Seq[StrLinkedRows])
  extends CrudSuccess(editorId, id.some, None, s"Read linked rows") with Read

case class ReadRow(editorId: EditorId, id: StrRowId, row: Option[StrTableRow])
  extends CrudSuccess(editorId, id.some, None, s"Read row") with Read

case class ReadTable(editorId: EditorId, rows: Seq[StrTableRow])
  extends CrudSuccess(editorId, None, None, s"Read ${rows.size} rows") with Read

case class Updated(editorId: EditorId, col: ColumnRef, id: StrRowId, oldValue: Option[StrValue], newValue: StrValue)
  extends CrudSuccess(editorId, id.some, col.some, s"Updated from ${oldValue.fold("empty")(f ⇒ s"«${f.value}»")} to «${newValue.value}»")

case class Deleted(editorId: EditorId, id: StrRowId)
  extends CrudSuccess(editorId, id.some, None, s"Deleted row")


sealed abstract class CrudFailure(editorId: EditorId, oid: Option[StrRowId], oc: Option[ColumnRef], e: ErrorMsg, failDesc: String) extends CrudResult(editorId, oid, oc) {
  override final def desc = s"$failDesc: ${e.value}"
}

case class CreateFailed (editorId: EditorId, ve: ErrorMsg \/ Seq[(ColumnRef, ErrorMsg)])
  extends CrudFailure(editorId, None, None, ve.fold(identity, es ⇒ ErrorMsg(es.map{case (col, e) ⇒ s"$col: ${e.value}"}.mkString(", "))), "Failed to create new row")

case class ReadRowFailed(editorId: EditorId, id: StrRowId, e: ErrorMsg)
  extends CrudFailure(editorId, id.some, None, e, "Failed to read row")

case class UpdateFailed (editorId: EditorId, col: ColumnRef, id: StrRowId, value: StrValue, e: ErrorMsg)
  extends CrudFailure(editorId, id.some, col.some, e, s"Failed to update row to value «${value.value}»")

case class DeleteFailed (editorId: EditorId, id: StrRowId, e: ErrorMsg)
  extends CrudFailure(editorId, id.some, None, e, s"Failed to delete row")

case class CrudException(editorId: EditorId, e: ErrorMsg, s: String)
  extends CrudFailure(editorId, None, None, e, s)

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