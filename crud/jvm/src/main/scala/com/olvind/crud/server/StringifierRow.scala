package com.olvind.crud
package server

import com.olvind.stringifiers.Stringifier

/**
  * A collection of `Stringifiers`s, one for each column of a database row.
  * This means that in order create an instance of this, you need `Cell`s for every
  * type in the row
  */
@annotation.implicitNotFound("Couldn't find Stringifier instances for all the types in projection ${P}")
trait StringifierRow[P] {
  def stringifiers: List[Stringifier[_]]

  /* given an instance of 'P', pick out the fields that correspond to each `Stringifier` in 'stringifiers' */
  def unpackValues(e: P): List[Any]

  /* reconstruct a P given a list of values */
  def packValues(a: Seq[Any]): P

  final override def toString = s"StringRow[${stringifiers.mkString(", ")}]"
}

object StringifierRow {
  /**
   * Use this to use tables mapped to a non-tuple structure.
   **/
  def apply[Mapped, Tupled <: Product : StringifierRow]
           (apply:   Tupled ⇒ Mapped,
            unapply: Mapped ⇒ Option[Tupled]): StringifierRow[Mapped] =
    new StringifierRow[Mapped] {
      val wrapped                           = implicitly[StringifierRow[Tupled]]
      override def packValues(vs: Seq[Any]) = apply(wrapped.packValues(vs))
      override def unpackValues(e: Mapped)  = wrapped.unpackValues(unapply(e).get)
      override def stringifiers             = wrapped.stringifiers
    }

  /**
   * Add support for tuples out of the box
   */
  trait StringifierRowProduct[P <: Product] extends StringifierRow[P] {
    def unpackValues(e: P): List[Any] = e.productIterator.toList
  }

  private val C = Stringifier
  private type C[E] = Stringifier[E]
  private def i[T](vs: Seq[Any], idx: Int): T = vs(idx).asInstanceOf[T]

  implicit def singular[A1:C]: StringifierRow[A1] = new StringifierRow[A1]{
    def stringifiers = List(C[A1])
    def packValues(vs: Seq[Any]) = vs.head.asInstanceOf[A1]
    def unpackValues(e: A1) = List(e)
  }

  implicit def tuple2[A1:C, A2:C]: StringifierRowProduct[(A1, A2)] =
    new StringifierRowProduct[(A1, A2)] {
      def stringifiers = List(C[A1], C[A2])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1))
    }

  implicit def tuple3[A1:C, A2:C, A3:C]: StringifierRowProduct[(A1, A2, A3)] =
    new StringifierRowProduct[(A1, A2, A3)] {
      def stringifiers = List(C[A1], C[A2], C[A3])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2))
    }

  implicit def tuple4[A1:C, A2:C, A3:C, A4:C]: StringifierRowProduct[(A1, A2, A3, A4)] =
    new StringifierRowProduct[(A1, A2, A3, A4)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3))
    }

  implicit def tuple5[A1:C, A2:C, A3:C, A4:C, A5:C]: StringifierRowProduct[(A1, A2, A3, A4, A5)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4))
    }

  implicit def tuple6[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5))
    }

  implicit def tuple7[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6))
    }

  implicit def tuple8[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7))
    }

  implicit def tuple9[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8))
    }

  implicit def tuple10[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9))
    }

  implicit def tuple11[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10))
    }

  implicit def tuple12[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C, A12:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11], C[A12])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11))
    }

  implicit def tuple13[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C, A12:C, A13:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11], C[A12], C[A13])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12))
    }

  implicit def tuple14[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C, A12:C, A13:C, A14:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11], C[A12], C[A13], C[A14])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13))
    }

  implicit def tuple15[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C, A12:C, A13:C, A14:C, A15:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11], C[A12], C[A13], C[A14], C[A15])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14))
    }

  implicit def tuple16[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C, A12:C, A13:C, A14:C, A15:C, A16:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11], C[A12], C[A13], C[A14], C[A15], C[A16])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15))
    }

  implicit def tuple17[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C, A12:C, A13:C, A14:C, A15:C, A16:C, A17:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11], C[A12], C[A13], C[A14], C[A15], C[A16], C[A17])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16))
    }

  implicit def tuple18[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C, A12:C, A13:C, A14:C, A15:C, A16:C, A17:C, A18:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11], C[A12], C[A13], C[A14], C[A15], C[A16], C[A17], C[A18])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16), i[A18](a, 17))
    }

  implicit def tuple19[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C, A12:C, A13:C, A14:C, A15:C, A16:C, A17:C, A18:C, A19:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11], C[A12], C[A13], C[A14], C[A15], C[A16], C[A17], C[A18], C[A19])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16), i[A18](a, 17), i[A19](a, 18))
    }

  implicit def tuple20[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C, A12:C, A13:C, A14:C, A15:C, A16:C, A17:C, A18:C, A19:C, A20:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11], C[A12], C[A13], C[A14], C[A15], C[A16], C[A17], C[A18], C[A19], C[A20])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16), i[A18](a, 17), i[A19](a, 18), i[A20](a, 19))
    }

  implicit def tuple21[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C, A12:C, A13:C, A14:C, A15:C, A16:C, A17:C, A18:C, A19:C, A20:C, A21:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11], C[A12], C[A13], C[A14], C[A15], C[A16], C[A17], C[A18], C[A19], C[A20], C[A21])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16), i[A18](a, 17), i[A19](a, 18), i[A20](a, 19), i[A21](a, 20))
    }

  implicit def tuple22[A1:C, A2:C, A3:C, A4:C, A5:C, A6:C, A7:C, A8:C, A9:C, A10:C, A11:C, A12:C, A13:C, A14:C, A15:C, A16:C, A17:C, A18:C, A19:C, A20:C, A21:C, A22:C]: StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] =
    new StringifierRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] {
      def stringifiers = List(C[A1], C[A2], C[A3], C[A4], C[A5], C[A6], C[A7], C[A8], C[A9], C[A10], C[A11], C[A12], C[A13], C[A14], C[A15], C[A16], C[A17], C[A18], C[A19], C[A20], C[A21], C[A22])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16), i[A18](a, 17), i[A19](a, 18), i[A20](a, 19), i[A21](a, 20), i[A22](a, 21))
    }
  }
