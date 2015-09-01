package com.olvind

import com.olvind.stringifiers.DecodeFail

import scala.language.implicitConversions

package object crud {
  private [crud] type RestrictedValues = Map[ColumnRef, Seq[StrValue]]
  private [crud] type ValidationError = (ColumnRef, Option[DecodeFail])

  @inline private [crud] final implicit class AnyX[A](private val a: A) extends AnyVal {
    /* type-safe equals */
    @inline def =:=(aa: A): Boolean = a == aa
    /* type-safe equals */
    @inline def =/=(aa: A): Boolean = a != aa

    /* some random syntax */

    @inline def some: Option[A] = Some(a)
    @inline def <|(f: A => Unit): A = {
      f(a)
      a
    }
    @inline def |>[B](f: A => B): B = f(a)
  }
}
