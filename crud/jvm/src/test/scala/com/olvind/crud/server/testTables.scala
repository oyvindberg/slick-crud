package com.olvind.crud.server

import com.typesafe.slick.testkit.util.JdbcTestDB

import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait testTables {
  val tdb: JdbcTestDB

  import tdb.driver.api._

  val db: tdb.driver.api.Database

  case class Name(value: String)
  case class Desc(value: String)
  case class Price(value: Int)

  case class StoreId(value: String)
  case class Store(id: StoreId, name: Name, description: Option[Desc], closed: Boolean)

  case class ProductId(id: Long)
  case class Product(id: ProductId, name: Name, quantity: Int, price: Price, soldBy: StoreId)

  implicit lazy val m1 = MappedColumnType.base[Desc,       String](_.value, Desc)
  implicit lazy val m2 = MappedColumnType.base[Name,       String](_.value, Name)
  implicit lazy val m3 = MappedColumnType.base[ProductId,  Long]  (  _.id,  ProductId)
  implicit lazy val m4 = MappedColumnType.base[StoreId,    String](_.value, StoreId)
  implicit lazy val m5 = MappedColumnType.base[Price,      Int]   (_.value, Price)

  implicit val c0 = Cell.instance[Name](Cell.nonEmpty andThen Name)(_.value)
  implicit val c1 = Cell.instance[Desc](Cell.nonEmpty andThen Desc)(_.value)
  implicit val c2 = Cell.instance[StoreId](Cell.nonEmpty andThen StoreId)(_.value)
  implicit val c3 = Cell[Long].xmap[ProductId](ProductId)(_.id)
  implicit val c4 = Cell[Int, Price](Price)(_.value)

  implicit val e1 = CellRow(Product.tupled,  Product.unapply)
  implicit val e2 = CellRow(Store.tupled,    Store.unapply)

  object t {
    abstract class StoreAbs[T](tag: Tag) extends Table[T](tag, "stores") {
      def id        = column[StoreId]("id")
      def name      = column[Name]   ("name")
      def descr     = column[Desc]   ("description", O.Nullable).?
      def closed    = column[Boolean]("closed")
    }
    class StoreT(tag: Tag) extends StoreAbs[Store](tag) {
      def *         = (id, name, descr, closed) <> (Store.tupled, Store.unapply)
    }
    class StoreTupledT(tag: Tag) extends StoreAbs[(StoreId, Name, Option[Desc], Boolean)](tag) {
      def *        = (id, name, descr, closed)
    }
    val Stores       = TableQuery[StoreT]
    val StoresTupled = TableQuery[StoreTupledT]

    abstract class ProductAbs[T](tag: Tag) extends Table[T](tag, "products") {
      def id        = column[ProductId]("id", O.PrimaryKey, O.AutoInc)
      def name      = column[Name]     ("name")
      def quantity  = column[Int]      ("quantity")
      def price     = column[Price]    ("price")
      def soldBy    = column[StoreId]  ("sold_by")
    }
    class ProductT(tag: Tag) extends ProductAbs[Product](tag) {
      def * = (id, name, quantity, price, soldBy) <> (Product.tupled, Product.unapply)
    }
    class ProductTupledT(tag: Tag) extends ProductAbs[(ProductId, Name, Int, Price, StoreId)](tag) {
      def * = (id, name, quantity, price, soldBy)
    }
    val Products       = TableQuery[ProductT]
    val ProductsTupled = TableQuery[ProductTupledT]

    val createTables: DBIO[Unit] =
      Products.schema.create >> Stores.schema.create

    Await.result(db.run(createTables), Duration.Inf)
  }
}