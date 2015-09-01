package com.olvind.crud

sealed trait CellRendering

object CellRendering {
  private[crud] case object Link       extends CellRendering
                case object Text       extends CellRendering
                case object Number     extends CellRendering
                case object Checkbox   extends CellRendering

  import upickle.default._

  /* the explicit declaration of these implicits is due to SI-7046,
      workaround found in https://github.com/lihaoyi/upickle/issues/68 */
  implicit val CellRenderingR: Reader[CellRendering] = Reader[CellRendering](
    implicitly[Reader[Link.type     ]].read orElse
    implicitly[Reader[Text.type     ]].read orElse
    implicitly[Reader[Number.type   ]].read orElse
    implicitly[Reader[Checkbox.type ]].read
  )

  implicit val CellRenderingW: Writer[CellRendering] = Writer[CellRendering] {
    case x: Link.type      ⇒ writeJs(x)
    case x: Text.type      ⇒ writeJs(x)
    case x: Number.type    ⇒ writeJs(x)
    case x: Checkbox.type  ⇒ writeJs(x)
  }
}
