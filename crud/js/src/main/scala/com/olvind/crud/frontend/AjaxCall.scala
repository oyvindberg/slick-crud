package com.olvind.crud
package frontend

import org.scalajs.dom.ext.Ajax
import upickle.default._

import scala.concurrent.Future

final case class AjaxCall(urlPrefix: String) extends autowire.Client[String, Reader, Writer] {

  override def doCall(req: Request): Future[String] =
    Ajax.post(
      url          = s"slick-crud-api/$urlPrefix/${req.path.mkString("/")}",
      data         = write(req.args),
      timeout      = 2000
    ).map(_.response.asInstanceOf[String])

  override def read [Result: Reader](p: String) = upickle.default.read[Result](p)
  override def write[Result: Writer](r: Result) = upickle.default.write[Result](r)
}