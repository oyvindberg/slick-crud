package com.olvind

import scala.language.implicitConversions

package object crud
  extends scalaz.syntax.ToEitherOps
  with scalaz.syntax.ToIdOps
  with scalaz.syntax.std.ToOptionOps
  with scalaz.syntax.std.ToOptionIdOps {

  /* import disjunction package-wide */
  private [crud] final val  \/  = scalaz.\/
  private [crud] final val  \/- = scalaz.\/-
  private [crud] final val -\/  = scalaz.-\/

  private [crud] final type  \/ [+A, +B] = scalaz.\/[A, B]
  private [crud] final type  \/-[    +B] = scalaz.\/-[B]
  private [crud] final type -\/ [+A    ] = scalaz.-\/[A]

  /* enable disjunction support in upickle */
  import upickle.default._

  private [crud] implicit def DisjR[A: Reader, B: Reader]: Reader[A \/ B] =
    Reader[A \/ B](implicitly[Reader[\/-[B]]].read orElse implicitly[Reader[-\/[A]]].read)

  private [crud] implicit def DisjW[A: Writer, B: Writer]: Writer[A \/ B] =
    Writer[A \/ B] {
      case x:  \/-[B] ⇒ writeJs(x)
      case x: -\/[A]  ⇒ writeJs(x)
    }

  /* remove the dreaded string plus operator on Object */
  private [crud] implicit def any2stringadd(x: Option[Unit]): Option[Unit] = x

  /* type-safe equals */
  @inline private [crud] implicit class AnyX[A](private val a: A) extends AnyVal {
    @inline def =:=(aa: A): Boolean = a == aa
    @inline def =/=(aa: A): Boolean = a != aa
  }

  private [crud] type RestrictedValues = Map[ColumnRef, Seq[StrValue]]
}
