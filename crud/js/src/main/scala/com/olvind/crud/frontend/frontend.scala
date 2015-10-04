package com.olvind
package crud

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom._

import scala.scalajs.js

package object frontend{
  private [frontend] type U[T] = js.UndefOr[T]

  private [frontend]val uNone = js.undefined

  @inline private [frontend] implicit final class IdToU[T](private val t: T) extends AnyVal {
    @inline def undef: U[T] = t
  }

  implicit private [frontend] class OptionalToU[M[_], T](private val mt: M[T]) extends AnyVal {
    @inline def asUndef(implicit O: OptionLike[M]): U[T] =
      O.fold[T, U[T]](mt, uNone)(t ⇒ t)
  }

  @inline private [frontend] implicit final class CBX(private val c: Callback) {
    @inline def filter(pred: Boolean): Callback =
      if (pred) c else Callback.empty
  }

  //using `js.UndefOr` under here to work around some strange implicit resolving behaviour

  @inline private [frontend] implicit final class UF0CB[R](private val uc: js.UndefOr[CallbackTo[R]]) extends AnyVal {
    @inline def voidU: Callback =
      uc.fold(Callback.empty)(_.void)
  }

  @inline private [frontend] implicit final class UF1CB[T1, R](private val uc: js.UndefOr[T1 ⇒ CallbackTo[R]]) extends AnyVal {
    @inline def mapply(t1: T1): U[CallbackTo[R]] =
      uc.map(f ⇒ f(t1))

    @inline def liftParam: T1 ⇒ U[CallbackTo[R]] =
      t1 ⇒ uc.map(f ⇒ f(t1))
  }

  @inline private [frontend] implicit final class UF2CB[T1, T2, R](private val uc: js.UndefOr[(T1, T2) ⇒ CallbackTo[R]]) extends AnyVal {
    @inline def mapply(t1: T1, t2: T2): U[CallbackTo[R]] =
      uc.map(f ⇒ f(t1, t2))

    @inline def liftParam: T1 ⇒ U[T2 ⇒ CallbackTo[R]] =
      t1 ⇒ uc.map(f ⇒ t2 ⇒ f(t1, t2))
  }

  @inline private [frontend] implicit final class UF2CB_[T1, T2, R](private val uc: js.UndefOr[T1 ⇒ T2 ⇒ CallbackTo[R]]) extends AnyVal {
    @inline def mapply(t1: T1, t2: T2): U[CallbackTo[R]] =
      uc.map(f ⇒ f(t1)(t2))

    @inline def liftParam: T1 ⇒ U[T2 ⇒ CallbackTo[R]] =
      t1 ⇒ uc.map(f ⇒ t2 ⇒ f(t1)(t2))
  }

  @inline private [frontend] implicit final class UF3CB[T1, T2, T3, R](private val uc: js.UndefOr[(T1, T2, T3) ⇒ CallbackTo[R]]) extends AnyVal {
    @inline def mapply(t1: T1, t2: T2, t3: T3): U[CallbackTo[R]] =
      uc.map(f ⇒ f(t1, t2, t3))

    @inline def liftParam: T1 ⇒ U[(T2, T3) ⇒ CallbackTo[R]] =
      t1 ⇒ uc.map(f ⇒ (t2, t3) ⇒ f(t1, t2, t3))
  }

  implicit private [frontend] class CallbackToCFutureX[T](c: CallbackTo[CFuture[T]]){
    def commit(f: T ⇒ Callback): Callback =
      c.map(_.commit(f)).void
  }

  implicit private [frontend] class OptionalAttrX(private val a: Attr) extends AnyVal {
    @inline def :=?[M[_], T](mt: M[T])(implicit ev: AttrValue[T], O: OptionLike[M]) =
      O.fold[T, U[TagMod]](mt, uNone)(t ⇒ a := t)
  }

  implicit private [frontend] object AttrValueStrValue extends AttrValue[StrValue]{
    override def apply(v: StrValue, b: (js.Any) ⇒ Unit): Unit = b(v.value)
  }

  implicit private [frontend] val executionContext =
    scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
}
