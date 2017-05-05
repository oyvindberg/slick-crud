package com.olvind.crud.server

import java.util.UUID

import com.olvind.crud._
import com.olvind.stringifiers.Stringifier
import com.typesafe.slick.testkit.util._
import org.junit.Assert
import slick.dbio.Effect.Write

class CrudTest
  extends TypedAsyncTest[JdbcTestDB]
  with testTables { outer ⇒

  object instance extends serverEditors with syntax with onThreadExecutionContexts {
    override lazy val driver = tdb.driver
    override lazy val db     = outer.db
  }

    /* just... dont put these in production code*/
  implicit def toStr[T](t: T)(implicit S: Stringifier[T]): StrValue =
    StrValue(S encode t)

  implicit def toIdStr[T](t: T)(implicit S: Stringifier[T]): StrRowId =
    StrRowId(S encode t)

  implicit def fromStr[T](s: StrValue)(implicit S: Stringifier[T]): T =
    (S decode s.value).right.toOption.get

  implicit class Meh[T](m: XRes[T]){
    def forceSuccess: T = m match {
      case XSuccess(t) ⇒ t
      case other       ⇒ Assert.fail(s"unexpected error: $other"); ???
    }
  }

  /* some test data */
  val editorName    = EditorName("asd")
  def eid           = EditorId(UUID.randomUUID.toString)
  val storeId       = StoreId("store")
  val ignore        = ProductId(Int.MaxValue)
  val n1            = Name("amazing product")
  val n2            = Name("nights")
  val n3            = Name("new name for product three")
  val q1            = 100
  val price         = Price(100)
  val u             = UserInfo("test")

  def rowContains(shouldContain: Boolean,
                  needle:        StrValue)
                 (haystack:      StrTableRow): Boolean =
    shouldContain == haystack.values.exists(_ =:= needle)

  implicit class XXX(haystack: Option[StrTableRow]) {
    def shouldContain(shouldContain: Boolean,
                      needle:        StrValue): Unit = {
      val ret = haystack match {
        case None      ⇒ !shouldContain
        case Some(row) ⇒ rowContains(shouldContain, needle)(row)
      }
      Assert.assertTrue(s"$haystack should ${if (shouldContain) "" else " not "} contain $needle", ret)
    }
  }

  implicit class YY(haystack: Seq[StrTableRow]) {
    def shouldContain(shouldContain: Boolean, needle: StrValue): Unit = {
      val ret = shouldContain == haystack.exists(rowContains(shouldContain, needle))
      Assert.assertEquals(s"$haystack should ${if (shouldContain) "" else " not "} contain $needle", shouldContain, ret)
    }
  }
  def expectedRow(id: StrRowId, data: StrValue*): StrTableRow =
    StrTableRow(id.some, data)

  def lookupCol(e: Editor)(s: String): ColumnRef =
    e.desc().columns.find(_.name.value =:= s).map(_.ref).get

  def lookupColForCreate(e: Editor)(s: String): ColumnRef =
    e.desc().mainCols.find(_.name.value =:= s).map(_.ref).get

  import tdb.driver.api._

  def productInsert(p: Product): tdb.driver.DriverAction[ProductId, NoStream, Write] =
    t.Products returning t.Products.map(_.id) += p

  val action = instance.crudAction

  def testView() = {
    val ref: instance.BaseTableRef[ProductId, t.ProductT] =
      instance.TableRef(t.Products)(_.id)

    for {
      pid1 ← t.Products += Product(ignore, n1, q1, price, storeId)
      pid2 ← t.Products += Product(ignore, n2, 100, price, storeId)
      _    ← t.Stores   += Store(storeId, Name("store"), None, closed = true)
      view ← action.readTable(ref, None).res
      _    = view.forceSuccess.shouldContain(shouldContain = true, n1)
    } yield ()
  }

  def testViewId() = {
    val ref: instance.BaseTableRef[ProductId, t.ProductT] =
      instance.TableRef(t.Products)(_.id)

    for {
      pid1 ← productInsert(Product(ignore, n1, q1, price, storeId))
      pid2 ← productInsert(Product(ignore, n2, 100, price, storeId))
      /* check that asking for one id only returns values for that product*/
      r0   ← action.readRow(ref, pid1).res
      _    = r0.forceSuccess shouldContain (shouldContain = true,  n1)
      r1   ← action.readRow(ref, pid1).res
      _    = r1.forceSuccess shouldContain (shouldContain = false, n2)
      r2   ← action.readRow(ref, pid2).res
      _    = r2.forceSuccess shouldContain (shouldContain = false, n1)
      r3   ← action.readRow(ref, pid2).res
      _    = r3.forceSuccess shouldContain (shouldContain = true,  n2)
      r4   ← action.readRow(ref, ProductId(-1)).res
      _    = r4.forceSuccess shouldContain (shouldContain = false, n1)
      r5   ← action.readRow(ref, ProductId(-1)).res
      _    = r5.forceSuccess shouldContain (shouldContain = false, n2)
    } yield ()

  }

  def testViewIdProjection() = {
    val ref: instance.TableRef[ProductId, t.ProductT, (Rep[ProductId], Rep[Int], Rep[Name]), (ProductId, Int, Name)] =
      instance.TableRef(t.Products)(_.id).projected(_.map(p ⇒ (p.id, p.quantity, p.name)))
    for {
      pid1 ← productInsert(Product(ignore, n1, q1, price, storeId))
      row  ← action.readRow(ref, pid1).res
      _    = row.forceSuccess.get shouldBe expectedRow(pid1, pid1, q1, n1)
    } yield ()
  }

  def testViewProjectionWithoutId() = {
    val ref: instance.TableRef[ProductId, t.ProductT, (Rep[Int], Rep[Name]), (Int, Name)] =
      instance.TableRef(t.Products)(_.id).projected(_.map(p ⇒ (p.quantity, p.name)))
    for {
      pid1  ← productInsert(Product(ignore, n1, q1, price, storeId))
      row   ← action.readRow(ref, pid1).res
      _     = row.forceSuccess.get shouldBe expectedRow(pid1, q1, n1)
    } yield ()
  }

  def testUpdateClassTuple() = {
    val ref: instance.TableRef[ProductId, t.ProductT, (Rep[ProductId], Rep[Name]), (ProductId, Name)] =
      instance.TableRef(t.Products)(_.id).projected(_.map(r ⇒ (r.id, r.name)))
    val e   = ref.build(eid, editorName)
    for {
      pid ← productInsert(Product(ignore, n2, 100, price, storeId))
      res ← action.update(ref, pid, lookupCol(e)("name"), n3).res
      row ← action.readRow(ref, pid).res
    } yield {
      res.forceSuccess shouldBe ((Some(n2), n3))
      row.forceSuccess.get shouldBe expectedRow(pid, pid, n3)
    }
  }

  def testUpdateTupleTuple() = {
    val ref: instance.TableRef[ProductId, t.ProductTupledT, (Rep[Int], Rep[Name]), (Int, Name)] =
      instance.TableRef(t.ProductsTupled)(_.id).projected(_.map(r ⇒ (r.quantity, r.name)))
    val e   = ref.build(eid, editorName)
    for {
      pid  ← productInsert(Product(ignore, n2, q1, price, storeId))
      res1 ← action.update(ref, pid, lookupCol(e)("name"), n3).res
      res2 ← action.readRow(ref, pid).res
    } yield {
      res1.forceSuccess shouldBe ((Some(n2), n3))
      res2.forceSuccess.get shouldBe expectedRow(pid, q1, n3)
    }
  }

  def testUpdateTupleSortedTuple() = {
    val ref: instance.TableRef[ProductId, t.ProductTupledT, (Rep[Int], Rep[Name]), (Int, Name)] =
      instance.TableRef(t.ProductsTupled)(_.id).projected(_.sortBy(_.quantity).map(r ⇒ (r.quantity, r.name)))
    val e   = ref.build(eid, editorName)
    for {
      pid  ← productInsert(Product(ignore, n2, q1, price, storeId))
      res1 ← action.update(ref, pid, lookupCol(e)("name"), n3).res
      res2 ← action.readRow(ref, pid).res
    } yield {
      res1.forceSuccess shouldBe ((Some(n2), n3))
      res2.forceSuccess.get shouldBe expectedRow(pid, q1, n3)
    }
  }

  def testUpdateClassClass() = {
    val ref: instance.BaseTableRef[ProductId, t.ProductT] =
      instance.TableRef(t.Products)(_.id)
    val e   = ref.build(eid, editorName)
    for {
      pid ← productInsert(Product(ignore, n2, q1, price, storeId))
      _   ← action.update(ref, pid, lookupCol(e)("name"), n3).res
      row ← action.readRow(ref, pid).res
      _   = row.forceSuccess.get shouldBe expectedRow(pid, pid, n3, q1, price, storeId)
    } yield ()
  }

  def testUpdateOnlyChosenColumns() = {
    val ref: instance.TableRef[ProductId, t.ProductT, (Rep[ProductId], Rep[StoreId]), (ProductId, StoreId)] =
      instance.TableRef(t.Products)(_.id).projected(_.map(r ⇒ (r.id, r.soldBy)))
    val e   = ref.build(eid, editorName)
    val col = ColumnRef(TableName("products"), ColumnName("name"), false)
    for {
      pid  ← productInsert(Product(ignore, n2, 100, price, storeId))
      fail ← action.update(ref, pid, col, n3).res
      _    = fail shouldBe XTechnicalMsg("table has no column name")
    } yield ()
  }

  def testUpdateOnlyValidId() = {
    val ref: instance.BaseTableRef[ProductId, t.ProductT] =
      instance.TableRef(t.Products)(_.id)
    val e   = ref.build(eid, editorName)
    for {
      pid ← productInsert(Product(ignore, n2, q1, price, storeId))
      res ← action.update(ref, ProductId(10001), lookupCol(e)("name"), n3).res
      _   = res shouldBe XTechnicalMsg("No rows matched")
    } yield ()
  }

  def testUpdateJoinedTable() = {
    val ref: instance.TableRef[ProductId, t.ProductT, (Rep[ProductId], Rep[Name], Rep[Int], Rep[Option[Name]]), (ProductId, Name, Int, Option[Name])] =
      instance.TableRef(t.Products)(_.id).projected(_.joinLeft(t.Stores).on(_.soldBy === _.id).map{
        case (p, sOpt) ⇒ (p.id, p.name, p.quantity, sOpt.map(_.name))
      })
    val e       = ref.build(eid, editorName)
    val col     = lookupCol(e)("name")
    for {
      pid  ← productInsert(Product(ignore, n2, 100, price, storeId))
      _    ← t.Stores += Store(storeId, Name("store"), None, closed = true)
      res  ← action.update(ref, pid, col, n3).res
      _    = res.forceSuccess.shouldBe((Some(n2), n3): (Option[StrValue], StrValue))
    } yield ()
  }


  def testUpdateOptionMappedColumn() = {
    class StoreT(tag: Tag) extends Table[(StoreId, Name, Option[Desc], Boolean)](tag, "stores") {
      def id       = column[StoreId]("id")
      def name     = column[Name]   ("name")
      def descr    = column[Desc]   ("description")
      val closed   = column[Boolean]("closed")
      def *        = (id, name, descr.?, closed) //<-- map desc to Option[Desc] in projection
    }

    val ref: instance.BaseTableRef[StoreId, StoreT] =
      instance.TableRef(TableQuery[StoreT])(_.id)

    val e      = ref.build(eid, editorName)
    val col    = lookupCol(e)("description")
    val sid    = StoreId("asdasdsad")

    for {
      _    ← TableQuery[StoreT] += ((sid, Name("fin butikk"), None, false))
      res1 ← action.update(ref, sid, col, "").res
      _    = res1.forceSuccess shouldBe ((Some(""), ""): (Option[StrValue], StrValue))
      res2 ← action.update(ref, sid, col, "arne").res
      _    = res2.forceSuccess shouldBe ((Some(""), "arne"): (Option[StrValue], StrValue))
    } yield ()
  }

  def testUpdateValueThatDidntExist() = {
    val ref: instance.BaseTableRef[StoreId, t.StoreTupledT] =
      instance.TableRef(t.StoresTupled)(_.id)

    val e   = ref.build(eid, editorName)
    val col = lookupCol(e)("description")
    val sid = StoreId("asdasdsad2")
    for {
      _   ← t.StoresTupled += ((sid, Name("fin butikk"), None, false))
      res ← action.update(ref.base, sid, col, "arne").res
      _   = res.forceSuccess shouldBe ((Some(""), "arne"): (Option[StrValue], StrValue))
    } yield ()
  }

  def testUpdateWhenIdColumnNotSelected() = {
    val ref: instance.TableRef[ProductId, t.ProductT, Rep[Name], Name] =
      instance.TableRef(t.Products)(_.id).projected(_.map(_.name))
    val e   = ref.build(eid, editorName)
    val col = lookupCol(e)("name")
    for {
      pid  ← productInsert(Product(ignore, n2, q1, price, storeId))
      res1 ← action.update(ref, pid, col, n3).res
      res2 ← action.readRow(ref, pid).res
      _    = res2.forceSuccess.get shouldBe expectedRow(pid, n3)
    } yield ()
  }

  def testCreateTupled() = {
    val ref: instance.BaseTableRef[ProductId, t.ProductTupledT] =
      instance.TableRef(t.ProductsTupled)(_.id)
    val e   = ref.build(eid, editorName)

    val row =
      Map[ColumnRef, StrValue](
        lookupColForCreate(e)("name")      → n1,
        lookupColForCreate(e)("quantity")  → q1,
        lookupColForCreate(e)("price")     → price,
        lookupColForCreate(e)("sold_by")   → storeId
      )

    for {
      idOpt ← action.create(ref, row).res
      _     = idOpt.forceSuccess should (_.isDefined)
    } yield ()
  }

  def testCreateClass() = {
    val ref: instance.TableRef[ProductId, t.ProductT, t.ProductT, Product] =
      instance.TableRef(t.Products)(_.id).projected(_.sortBy(_.name))
    val e   = ref.build(eid, editorName)

    val quantity = 256
    val row =
      Map[ColumnRef, StrValue](
        lookupColForCreate(e)("name")      → n1,
        lookupColForCreate(e)("quantity")  → quantity,
        lookupColForCreate(e)("price")     → price,
        lookupColForCreate(e)("sold_by")   → storeId
      )

    for {
      id    ← action.create(ref.base, row).res
      r     ← action.readRow(ref, id.forceSuccess.get).res
      _     = r.forceSuccess.shouldContain(shouldContain = true, quantity)
    } yield ()
  }

  def testCreateNeedsAllColumns() = {
    val ref: instance.TableRef[ProductId, t.ProductTupledT, (Rep[Name], Rep[Int], Rep[StoreId]), (Name, Int, StoreId)] =
      instance.TableRef(t.ProductsTupled)(_.id).projected(_.map(r ⇒ (r.name, r.quantity, r.soldBy)))
    val e   = ref.build(eid, editorName)

    val row =
      Map[ColumnRef, StrValue](
        lookupColForCreate(e)("quantity")  → q1,
        lookupColForCreate(e)("sold_by")   → storeId
      )
    for {
      res <- action.create(ref.base, row).res
      _   = res shouldBe XValidation(None, List(
              (ColumnRef(TableName("products"), ColumnName("name"),  false),None),
              (ColumnRef(TableName("products"), ColumnName("price"), false),None)
            ))
    } yield ()
  }

  def testCreateNoAutoIncrement() = {
    val ref: instance.BaseTableRef[StoreId, t.StoreT] =
      instance.TableRef(t.Stores)(_.id)
    val e   = ref.build(eid, editorName)

    val sid = StoreId("storeId")
    val row =
      Map[ColumnRef, StrValue](
        lookupColForCreate(e)("id")          → sid,
        lookupColForCreate(e)("name")        → "my store",
        lookupColForCreate(e)("description") → storeId,
        lookupColForCreate(e)("closed")      → true
      )

    for {
      oid ← action.create(ref, row).res
      _   = oid.forceSuccess shouldBe Some(sid: StrRowId)
    } yield ()
  }

  def testDelete() = {
    val ref: instance.BaseTableRef[ProductId, t.ProductT] =
      instance.TableRef(t.Products, canDelete = true)(_.id)
    for {
      pid   ← productInsert(Product(ignore, n1, q1, price, storeId))
      row   ← action.readRow(ref, pid).res
      _     = row.forceSuccess.get shouldBe expectedRow(pid, pid, n1, q1, price, storeId)
      _     ← action.delete(ref, pid).res
      fail  ← action.readRow(ref, pid).res
      _     = fail.forceSuccess shouldBe None
    } yield ()
  }

  def testDeleteNotAllowed() = {
    val ref: instance.BaseTableRef[ProductId, t.ProductT] =
      instance.TableRef(t.Products)(_.id)
    for {
      pid  ← productInsert(Product(ignore, n1, q1, price, storeId))
      fail ← action.delete(ref, pid).res
      _    = fail shouldBe XTechnicalMsg("Can not delete from table")
      row  ← action.readRow(ref, pid).res
    } yield ()
  }
}
