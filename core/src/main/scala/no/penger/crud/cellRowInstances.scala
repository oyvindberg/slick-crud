package no.penger.crud

trait cellRowInstances extends cells {

  /**
   * Use this to use tables mapped to a non-tuple structure.
   **/
  def mappedCellRow[Mapped, Tupled <: Product : CellRow](unapply: Mapped => Option[Tupled]) =
    new CellRow[Mapped] {
      private val wrapped          = implicitly[CellRow[Tupled]]
      override def list(e: Mapped) = wrapped.list(unapply(e).get)
      override def cells           = wrapped.cells
    }

  /**
   * Add support for tuples out of the box
   */
  trait CellRowProduct[P <: Product] extends CellRow[P] {
    def list(e: P): List[Any] = e.productIterator.toList
  }

  private def c[A: Cell]: Cell[A] = implicitly[Cell[A]]

  implicit def tuple2[A1: Cell, A2: Cell]: CellRowProduct[(A1, A2)] =
    new CellRowProduct[(A1, A2)] {
      def cells = List(c[A1], c[A2])
    }

  implicit def tuple3[A1: Cell, A2: Cell, A3: Cell]: CellRowProduct[(A1, A2, A3)] =
    new CellRowProduct[(A1, A2, A3)] {
      def cells = List(c[A1], c[A2], c[A3])
    }

  implicit def tuple4[A1: Cell, A2: Cell, A3: Cell, A4: Cell]: CellRowProduct[(A1, A2, A3, A4)] =
    new CellRowProduct[(A1, A2, A3, A4)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4])
    }

  implicit def tuple5[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell]: CellRowProduct[(A1, A2, A3, A4, A5)] =
    new CellRowProduct[(A1, A2, A3, A4, A5)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5])
    }

  implicit def tuple6[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6])
    }

  implicit def tuple7[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7])
    }

  implicit def tuple8[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8])
    }

  implicit def tuple9[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9])
    }

  implicit def tuple10[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10])
    }

  implicit def tuple11[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11])
    }

  implicit def tuple12[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12])
    }

  implicit def tuple13[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13])
    }

  implicit def tuple14[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14])
    }

  implicit def tuple15[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15])
    }

  implicit def tuple16[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16])
    }

  implicit def tuple17[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17])
    }

  implicit def tuple18[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18])
    }

  implicit def tuple19[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19])
    }

  implicit def tuple20[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell, A20: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19], c[A20])
    }

  implicit def tuple21[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell, A20: Cell, A21: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19], c[A20], c[A21])
    }

  implicit def tuple22[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell, A20: Cell, A21: Cell, A22: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19], c[A20], c[A21], c[A22])
    }
}
