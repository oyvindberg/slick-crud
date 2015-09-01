package com.olvind.crud

case class ErrorMsg(value: String)

object ErrorMsg{
  def apply(th: Throwable): ErrorMsg = ErrorMsg(str(th))

  def tryCatch[T](t: ⇒ T)(f: Throwable ⇒ String = str): ErrorMsg \/ T =
    \/.fromTryCatchNonFatal(t) leftMap (f andThen ErrorMsg.apply)

  def str(t: Throwable) =
    s"${t.getClass.getSimpleName }: ${t.getMessage}"
}
