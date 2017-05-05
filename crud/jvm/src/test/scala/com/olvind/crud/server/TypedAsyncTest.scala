package com.olvind.crud.server

import com.typesafe.slick.testkit.util.{AsyncTest, TestDB}
import org.junit.Assert

import scala.reflect.ClassTag

abstract class TypedAsyncTest[TDB >: Null <: TestDB](implicit TdbClass: ClassTag[TDB]) extends AsyncTest[TDB] {
  //shadow
  implicit val AssertionExtensionMethods = ()

  implicit class AssertionExtensionMethodsTyped[T](v: T) {
    private[this] val cln = getClass.getName
    private[this] def fixStack(f: => Unit): Unit = try f catch {
      case ex: AssertionError =>
        ex.setStackTrace(ex.getStackTrace.iterator.filterNot(_.getClassName.startsWith(cln)).toArray)
        throw ex
    }

    def shouldBe(o: T): Unit =
      fixStack(Assert.assertEquals(o, v))

    def shouldNotBe(o: T): Unit =
      fixStack(Assert.assertNotSame(o, v))

    def should(f: T => Boolean): Unit = fixStack(Assert.assertTrue(f(v)))
  }
}
