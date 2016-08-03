package com.olvind
package crud

import com.olvind.stringifiers.DecodeFail
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactAttr.ValueType
import japgolly.scalajs.react.vdom._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContextExecutor
import scala.scalajs.js

package object frontend
  extends ReusabilityInstances
  with chandu0101.scalajs.react.components.Implicits {

  type ValidationError = Option[DecodeFail]

  @inline implicit class OptionalToU[T](private val ot: Option[T]) extends AnyVal {
    @inline def asUndef: js.UndefOr[T] = ot.fold[js.UndefOr[T]](js.undefined)(t ⇒ t)
  }

  @inline implicit class OptionalAttrX(private val a: ReactAttr) extends AnyVal {
    @inline def :=?[M[_], T](mt: M[T])(implicit ev: ValueType[T], O: OptionLike[M]): TagMod =
      O.fold[T, TagMod](mt, EmptyTag)(t ⇒ a := t)
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
