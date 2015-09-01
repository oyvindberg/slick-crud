package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.~=>
import org.scalajs.dom.ext.Ajax
import upickle.default._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object AjaxCall {
  def forTable(t: TableName) = AjaxCall(t.value)
}

case class AjaxCall(urlPrefix: String) extends autowire.Client[String, Reader, Writer] {

  override def doCall(req: Request): Future[String] =
    Ajax.post(
      url          = s"slick-crud-api/$urlPrefix/${req.path.mkString("/")}",
      data         = write(req.args),
      timeout      = 40000
    ).map(_.response.asInstanceOf[String])

  override def read [Result: Reader](p: String) = upickle.default.read[Result](p)
  override def write[Result: Writer](r: Result) = upickle.default.write[Result](r)
}

case class CFuture[T] private (underlying: Future[T]) extends AnyVal {
  def commit(f: T ⇒ Callback): Callback = {
    underlying foreach (t ⇒ f(t).runNow())
    Callback.empty
  }
  
  def currentValueU: U[T] =
    underlying.value.flatMap(_.toOption).asUndef
  
  def map[TT](f: T ⇒ TT): CFuture[TT] =
    CFuture(underlying map f)
  
  def flatMap[TT](f: T ⇒ CFuture[TT]): CFuture[TT] =
    CFuture(underlying flatMap(t ⇒ f(t).underlying))
}

case class AsyncCallback(
  onResult:  TimedT[CrudResult] ~=> Callback,
  onFailure: CrudFailure ⇒ Callback,
  tableName: TableName){

  def apply[S <: CrudSuccess]
           (errorDesc: String,
            _f:        ⇒ Future[CrudFailure \/ S]): CallbackTo[CFuture[S]] =

  CallbackTo[CFuture[S]](
    Clock { c ⇒
      val f: Future[CrudFailure \/ S] =
        Try(_f) match {
          case Success(ok) ⇒ ok
          case Failure(th) ⇒ Future.successful(CrudException(tableName, ErrorMsg(th), "Couldn't connect").left)
        }

      f.recover[CrudFailure \/ S]{
        case th ⇒ CrudException(tableName, ErrorMsg(th), errorDesc).left
      }

      f.onSuccess {
        case -\/(failure) ⇒
          (onResult(c.timed(failure)) >> onFailure(failure)).runNow()
        case  \/-(success)   ⇒
          onResult(c.timed(success)).runNow()
      }

      CFuture(f collect {case \/-(v) ⇒ v})
    }
  )
}