package com.olvind.crud

sealed trait SortOrder
object Asc  extends SortOrder
object Desc extends SortOrder

object SortOrder{
  import upickle.default._

  /* the explicit declaration of these implicits is due to SI-7046,
      workaround found in https://github.com/lihaoyi/upickle/issues/68 */
  implicit val SortOrderR: Reader[SortOrder] = Reader[SortOrder](
    implicitly[Reader[Asc.type ]].read orElse
    implicitly[Reader[Desc.type]].read
  )

  implicit val SortOrderW: Writer[SortOrder] = Writer[SortOrder] {
    case x: Asc.type  ⇒ writeJs(x)
    case x: Desc.type ⇒ writeJs(x)
  }
}