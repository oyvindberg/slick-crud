package com.olvind.crud
package frontend

import autowire.ClientProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.~=>
import org.scalajs.dom.ext.Ajax
import upickle.default
import upickle.default._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object AjaxCall {
  def forEditor(id: EditorId) = AjaxCall(id.value)
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

case class CFuture[T] private (underlying: Future[T]) extends AnyVal {
  def commit(f: T ⇒ Callback): Callback = {
    underlying foreach (t ⇒ f(t).runNow())
    Callback.empty
  }

  def currentValueU: U[T] =
    underlying.value.flatMap(_.toOption).asUndef

  def map[TT](f: T ⇒ TT): CFuture[TT] =
    CFuture(underlying map f)

  def foreach[TT](f: T ⇒ TT): Unit =
    underlying foreach f

  def flatMap[TT](f: T ⇒ CFuture[TT]): CFuture[TT] =
    CFuture(underlying flatMap(t ⇒ f(t).underlying))
}

case class AsyncCallback(
  remote:    ClientProxy[Editor, String, default.Reader, default.Writer],
  user:      UserInfo,
  onResult:  TimedT[CrudResult] ~=> Callback,
  onFailure: CrudFailure ⇒ Callback,
  editorId:  EditorId){

  private def go[T](toResult:  T => CrudResult,
                     errorDesc: String,
                     _f:        ⇒ ClientProxy[Editor, String, default.Reader, default.Writer] => UserInfo => Future[T]): CallbackTo[CFuture[T]] =

    CallbackTo[CFuture[T]](
      Clock { c ⇒
        val f: Future[CrudException \/ T] =
          Try(_f(remote)(user)) match {
            case Failure(th) ⇒
              val e = CrudException(editorId, ErrorMsg(th), "Couldn't connect")
              (onResult(c.timed(e)) >> onFailure(e)).runNow()
              Future.successful(e.left)
            case Success(ok) ⇒
              ok.map(_.right)
          }

        f recover { case th => CrudException(editorId, ErrorMsg(th), errorDesc).left }

        f.onSuccess {
          case  \/-(t) => onResult(c.timed(toResult(t))).runNow()
          case -\/ (e) => (onResult(c.timed(e)) >> onFailure(e)).runNow()
        }

        CFuture(f collect {case \/-(v) ⇒ v})
      }
    )

  def apply[S <: CrudSuccess]
           (errorDesc:  String,
            _f: ⇒ ClientProxy[Editor, String, default.Reader, default.Writer] => UserInfo => Future[S]): CallbackTo[CFuture[S]] =

    go(identity, errorDesc, _f)

  def applyEither[F <: CrudFailure, S <: CrudSuccess]
                 (errorDesc:  String,
                  _f: ⇒       ClientProxy[Editor, String, default.Reader, default.Writer] => UserInfo => Future[F \/ S])
                 (onError:    F => Callback = onFailure) = {

    def toResult(e: F \/ S): CrudResult = e.fold(identity, identity)

    go(toResult, errorDesc, _f).map {
      f =>
        f.foreach {
          case -\/ (error) => onError(error).runNow()
          case  \/-(ok)    => ()
        }
        CFuture(f.underlying collect {case \/-(v) ⇒ v})
    }
  }
}