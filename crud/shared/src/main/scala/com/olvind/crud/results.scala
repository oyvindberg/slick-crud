package com.olvind.crud

import com.olvind.stringifiers.DecodeFail

final case class Coordinate (
  editorId: EditorId,
  rowOpt:   Option[StrRowId],
  colOpt:   Option[ColumnRef]) {

  def formatted: String = {
    val title       = s"Editor ${editorId.value}"
    val colTitleOpt = colOpt.map(c ⇒ s"column ${c.name.value} in table ${c.table.value}")
    val idOpt       = rowOpt.map(id ⇒ s"id ${id.value}")
    ((title +: colTitleOpt.toList) ++ idOpt.toList).mkString("", ", ", ": ")
  }
}

sealed trait XRes[+T]{
  final def map[R](f: T => R): XRes[R] =
    this match {
      case XSuccess(t) => XSuccess(f(t))
      case f: XFail    => f
    }
}

sealed trait XFail extends XRes[Nothing]
final case class XValidation(idOpt: Option[StrRowId], vs: Seq[(ColumnRef, Option[DecodeFail])]) extends XFail
final case class XUserMsg(value: String) extends XFail
final case class XTechnicalMsg(value: String) extends XFail
final case class XSuccess[T](t: T) extends XRes[T]

object XTechnicalMsg {
  def apply(th: Throwable): XTechnicalMsg =
    XTechnicalMsg(str(th))

  def str(t: Throwable) =
    s"${t.getClass.getSimpleName }: ${t.getMessage}"
}

object XRes {
  import upickle.default._

  /* the explicit declaration of these implicits is due to SI-7046,
      workaround found in https://github.com/lihaoyi/upickle/issues/68 */
  implicit def XResR[T: Reader]: Reader[XRes[T]] =
    Reader[XRes[T]](
      implicitly[Reader[XValidation  ]].read orElse
      implicitly[Reader[XUserMsg     ]].read orElse
      implicitly[Reader[XTechnicalMsg]].read orElse
      implicitly[Reader[XSuccess[T]  ]].read
    )

  implicit def XResW[T: Writer]: Writer[XRes[T]] =
    Writer[XRes[T]] {
      case x: XValidation   ⇒ writeJs(x)
      case x: XUserMsg      ⇒ writeJs(x)
      case x: XTechnicalMsg ⇒ writeJs(x)
      case x: XSuccess[T]   ⇒ writeJs(x)
    }
}