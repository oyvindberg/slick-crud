package com.olvind
package crud

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactAttr.ValueType
import japgolly.scalajs.react.vdom._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContextExecutor
import scala.scalajs.js

package object frontend extends chandu0101.scalajs.react.components.Implicits {
  type TimedRes = TimedT[(Coordinate, XRes[Any], Option[String])]
  type U[T] = js.UndefOr[T]

  @inline implicit final class IdToU[T](private val t: T) extends AnyVal {
    @inline def uSome: U[T] = t
  }

  implicit class OptionalToU[M[_], T](private val mt: M[T]) extends AnyVal {
    @inline def asUndef(implicit O: OptionLike[M]): U[T] =
      O.fold[T, U[T]](mt, js.undefined)(t ⇒ t)
  }

  //using `js.UndefOr` under here to work around some strange implicit resolving behaviour

  @inline implicit final class UF1CB[T1, R](private val uc: js.UndefOr[T1 ⇒ CallbackTo[R]]) extends AnyVal {
    @inline def mapply(t1: T1): U[CallbackTo[R]] =
      uc.map(f ⇒ f(t1))

    @inline def liftParam: T1 ⇒ U[CallbackTo[R]] =
      t1 ⇒ uc.map(f ⇒ f(t1))
  }

  @inline implicit final class UF2CB[T1, T2, R](private val uc: js.UndefOr[(T1, T2) ⇒ CallbackTo[R]]) extends AnyVal {
    @inline def mapply(t1: T1, t2: T2): U[CallbackTo[R]] =
      uc.map(f ⇒ f(t1, t2))

    @inline def liftParam: T1 ⇒ U[T2 ⇒ CallbackTo[R]] =
      t1 ⇒ uc.map(f ⇒ t2 ⇒ f(t1, t2))
  }

  @inline implicit final class UF2CB_[T1, T2, R](private val uc: js.UndefOr[T1 ⇒ T2 ⇒ CallbackTo[R]]) extends AnyVal {
    @inline def mapply(t1: T1, t2: T2): U[CallbackTo[R]] =
      uc.map(f ⇒ f(t1)(t2))

    @inline def liftParam: T1 ⇒ U[T2 ⇒ CallbackTo[R]] =
      t1 ⇒ uc.map(f ⇒ t2 ⇒ f(t1)(t2))
  }

  @inline implicit final class UF3CB[T1, T2, T3, R](private val uc: js.UndefOr[(T1, T2, T3) ⇒ CallbackTo[R]]) extends AnyVal {
    @inline def mapply(t1: T1, t2: T2, t3: T3): U[CallbackTo[R]] =
      uc.map(f ⇒ f(t1, t2, t3))

    @inline def liftParam: T1 ⇒ U[(T2, T3) ⇒ CallbackTo[R]] =
      t1 ⇒ uc.map(f ⇒ (t2, t3) ⇒ f(t1, t2, t3))
  }

  @inline implicit class OptionalAttrX(private val a: ReactAttr) extends AnyVal {
    @inline def :=?[M[_], T](mt: M[T])(implicit ev: ValueType[T], O: OptionLike[M]): U[TagMod] =
      O.fold[T, U[TagMod]](mt, js.undefined)(t ⇒ a := t)
  }

  implicit val ValueTypeStrValue: ValueType[StrValue] =
    ValueType[StrValue]((b, v) ⇒ b(v.value))

  implicit val executionContext: ExecutionContextExecutor =
    scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  /** stolen from scalaz */
  final def intersperse[A](as: List[A], a: A): List[A] = {
    @tailrec
    def intersperse0(accum: List[A], rest: List[A]): List[A] = rest match {
      case Nil      => accum
      case x :: Nil => x :: accum
      case h :: t   => intersperse0(a :: h :: accum, t)
    }
    intersperse0(Nil, as).reverse
  }
}
