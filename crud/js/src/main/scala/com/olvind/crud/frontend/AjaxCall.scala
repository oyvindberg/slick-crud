package com.olvind.crud
package frontend

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.~=>
import org.scalajs.dom.ext.Ajax
import upickle.default._

import scala.concurrent.Future

object AjaxCall {
  def forEditor(id: EditorId): AjaxCall =
    AjaxCall(id.value)
}

case class AjaxCall(urlPrefix: String) extends autowire.Client[String, Reader, Writer] {

  override def doCall(req: Request): Future[String] =
    Ajax.post(
      url          = s"slick-crud-api/$urlPrefix/${req.path.mkString("/")}",
      data         = write(req.args),
      timeout      = 2000
    ).map(_.response.asInstanceOf[String])

  override def read [Result: Reader](p: String) = upickle.default.read[Result](p)
  override def write[Result: Writer](r: Result) = upickle.default.write[Result](r)
}

case class AsyncCallback(
  user:      UserInfo,
  onResult:  TimedRes ~=> Callback,
  editorId:  EditorId
)