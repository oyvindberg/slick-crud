package com.olvind.crud.demo

import com.olvind.crud.server._
import com.olvind.stringifiers.Stringifier
import com.typesafe.scalalogging.LazyLogging
import slick.dbio.DBIOAction
import slick.dbio.Effect.All
import slick.driver.H2Driver
import unfiltered.filter.Plan
import unfiltered.jetty.Server

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Properties

trait StoreDomain{
  case class Name(value: String)
  case class Desc(value: String)
  case class Price(value: Int){
    require(value >= 0)
  }

  case class StoreId(value: String)
  case class Store(id: StoreId, name: Name, description: Option[Desc], closed: Boolean)

  case class ProductId(id: Long)
  case class Product(id: ProductId, name: Name, quantity: Int, price: Price, soldBy: StoreId)

  case class EmployeeId(id: Long)
  case class Employee(id: EmployeeId, name: Name, worksAt: Option[StoreId], role: Option[Role], good: Option[Boolean])

  sealed abstract class Role(val name: String)

  object Role{
    case object Employee extends Role("employee")
    case object Manager extends Role("manager")

    val values = List(Employee, Manager)
    def get(s: String): Role = s match {
      case Employee.name ⇒ Employee
      case Manager.name  ⇒ Manager
    }
  }
}

trait StoreTables extends StoreDomain with GenDataModule with integrationSlick with executionContexts {
  import driver.api._
  import MappedColumnType.base

  /* these type class instances are to enable the use of the types in slick */
  implicit lazy val m1 = base[Desc,       String](_.value, Desc)
  implicit lazy val m2 = base[EmployeeId, Long]  (  _.id,  EmployeeId)
  implicit lazy val m3 = base[Name,       String](_.value, Name)
  implicit lazy val m4 = base[ProductId,  Long]  (  _.id,  ProductId)
  implicit lazy val m5 = base[StoreId,    String](_.value, StoreId)
  implicit lazy val m6 = base[Role,       String](_.name,  Role.get)
  implicit lazy val m7 = base[Price,      Int]   (_.value, Price)

  class StoreT(tag: Tag) extends Table[Store](tag, "stores") {
    def id        = column[StoreId]("id", O.PrimaryKey)
    def name      = column[Name]   ("name")
    def descr     = column[Desc]   ("description", O.Nullable).?
    def closed    = column[Boolean]("closed")

    def *         = (id, name, descr, closed) <> (Store.tupled, Store.unapply)
  }
  val Stores = TableQuery[StoreT]

  class StoreNickNamesT(tag: Tag) extends Table[(StoreId, String)](tag, "store_nicknames"){
    def id       = column[StoreId]("id", O.PrimaryKey)
    def nickname = column[String]("nickname")

    def *        = (id, nickname)
    def fk1      = foreignKey("store_nicknames__store_fk", id, Stores)(_.id, onDelete = ForeignKeyAction.Cascade)
  }
  val StoreNickNames = TableQuery[StoreNickNamesT]

  class ProductT(tag: Tag) extends Table[Product](tag, "products") {
    def id        = column[ProductId]("id", O.PrimaryKey, O.AutoInc)
    def name      = column[Name]     ("name")
    def quantity  = column[Int]      ("quantity")
    def price     = column[Price]    ("price")
    def soldBy    = column[StoreId]  ("sold_by")

    def *         = (id, name, quantity, price, soldBy) <> (Product.tupled, Product.unapply)
    def fk1       = foreignKey("products__store_fk", soldBy, Stores)(_.id, onDelete = ForeignKeyAction.SetNull)
  }
  val Products = TableQuery[ProductT]

  class EmployeeT(tag: Tag) extends Table[Employee](tag, "employees"){
    def id        = column[EmployeeId]("id", O.PrimaryKey, O.AutoInc)
    def name      = column[Name]      ("name")
    def worksAt   = column[StoreId]   ("works_at", O.Nullable).?
    def role      = column[Role]      ("role", O.Nullable).?
    def good      = column[Boolean]   ("good", O.Nullable).?

    def *         = (id, name, worksAt, role, good) <> (Employee.tupled, Employee.unapply)
    def fk1       = foreignKey("employees__store_fk", worksAt, Stores)(_.id.?, onDelete = ForeignKeyAction.SetNull)
  }
  val Employees  = TableQuery[EmployeeT]

  lazy val init: DBIO[Unit] = for {
    _ ← Stores.schema.create
    _ ← StoreNickNames.schema.create
    _ ← Employees.schema.create
    _ ← Products.schema.create

    _ ← Stores         ++= GenData.stores
    _ ← StoreNickNames ++= GenData.storeNicknames
    _ ← Employees      ++= GenData.employees
    _ ← Products       ++= GenData.products
  } yield ()

}

trait StoreCrudInstances extends StoreDomain {
  /**
   * we need to provide Stringifier instances for every type we expose through slick-crud,
   *  in order for it to know how to render and parse them, analogously to slick
   */

  implicit val SName       = Stringifier.instance(Name)(_.value)
  implicit val SDesc       = Stringifier.instance(Desc)(_.value)
  implicit val SStoreId    = Stringifier.instance(StoreId)(_.value)
  implicit val SProductId  = Stringifier.instance(ProductId)(_.id)
  implicit val SEmployeeId = Stringifier.instance(EmployeeId)(_.id)
  implicit val SPrice      = Stringifier.instance(Price)(_.value)

  /* this defines a a type with a limited number of options */
  implicit val SRole = Stringifier.instance(Role.get)(_.name).withEnumValues(Role.values.toSet)

  /**
   * These StringifierRow mapping instances are necessary in order to expose
   *  tables that have default projections to non-tuple structures.
   */
  implicit val e1 = StringifierRow(Employee.tupled, Employee.unapply)
  implicit val e2 = StringifierRow(Product.tupled,  Product.unapply)
  implicit val e3 = StringifierRow(Store.tupled,    Store.unapply)
}


/* This demonstrates the wiring that you need to do in your application to get it working */
object CrudDemoWebApp extends StoreTables with StoreCrudInstances { outer ⇒

  override implicit val executionContext = ExecutionContext.Implicits.global
  override lazy     val driver           = H2Driver

  lazy val db = driver.api.Database.forURL(
    url = s"jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",
    driver = "org.h2.Driver"
  )

  object Demo
    extends integrationUnfiltered
    with syntax{

    override          val db               = outer.db
    override lazy     val driver           = outer.driver
    override implicit val executionContext = outer.executionContext

    import driver.api._

    Await.result(db.run(init), Duration.Inf)

    val storesBaseRef = TableRef(Stores, isEditable = true, canDelete = true)(_.id)

                        // It's not neccessary to explicitly tag types,
                        // but it makes IDEs behave better
    lazy val storesEd: ServerEditor[StoreId, StoreT, (Rep[StoreId], Rep[Name], Rep[Option[Desc]], Rep[Boolean], Rep[Option[String]], Rep[Option[Long]]), (StoreId, Name, Option[Desc], Boolean, Option[String], Option[Long])] =
        TableRefOps(storesBaseRef)
        // include a column from another table when we display. This will not
        // affect creating new rows, and the linked columns will not be editable
        .projected(
          _.joinLeft(StoreNickNames).on(_.id === _.id)
           .map{ case (s, od) ⇒ (s.id, s.name, s.descr, s.closed, od.map(_.nickname)) }
         )
        .projected(
          _.joinLeft(Products).on(_._1 === _.soldBy)
           .groupBy(_._1)
           .map {
             case ((c1, c2, c3, closed, c5), productsForStore) ⇒
               val productsSum: Rep[Option[Long]] =
                Case.If(!closed).Then[Option[Long], Long](
                 productsForStore.map(_._2.map(
                   p ⇒ p.price.asColumnOf[Long] * p.quantity.asColumnOf[Long]
                 )).sum).Else(None)
               (c1, c2, c3, closed, c5, productsSum)
            }
        )
        //sort the table by name when we display it initially
        .projected(_.sortBy(_._2))
        //bind to another table on this tables' storeId
        .linkedOn(_._1, employeeEd)(_.worksAt)(_ === _)
        .linkedOn(_._1, productsEd)(_._2)(_ === _)
        .linkedOn(_._1, storeNickNamesEd)(_.id)(_ === _)
        .build(EditorId("stores"), EditorName("Stores"))

    lazy val employeeRef: TableRef[EmployeeId, EmployeeT, EmployeeT, Employee] =
      TableRef(Employees, isEditable = true, canDelete = true)(_.id)
        .projected(_.sortBy(_.name.asc))
        .linkedOn(_.worksAt, storesEd)(_._1)(_ === _)

    lazy val employeeEd: ServerEditor[EmployeeId, EmployeeT, EmployeeT, Employee] =
      employeeRef.build(EditorId("all-employees"), EditorName("Employees"))

    lazy val productsEd: ServerEditor[ProductId, ProductT, (Rep[ProductId], Rep[StoreId], Rep[Int], Rep[Price], Rep[Name], Rep[Long]), (ProductId, StoreId, Int, Price, Name, Long)] =
      TableRef(Products, canDelete = true)(_.id)
        .projected(_.map(t ⇒ (t.id, t.soldBy, t.quantity, t.price, t.name, t.quantity.asColumnOf[Long] * t.price.asColumnOf[Long])))
        .linkedOn(_._2, storesEd)(_._1)(_ === _)
        .build(EditorId("products"), EditorName("Products"))

    lazy val storeNickNamesEd: ServerEditor[StoreId, StoreNickNamesT, StoreNickNamesT, (StoreId, String)] =
      TableRef(StoreNickNames, canDelete = true)(_.id)
        .linkedOn(_.id, storesEd)(_._1)(_ === _)
        .build(EditorId("store-nicknames"), EditorName("Store nicknames"))

    val productsForEmployeeEd: ServerEditor[EmployeeId, EmployeeT, (Rep[EmployeeId], Rep[Name], Rep[Option[StoreId]], Rep[Option[Boolean]], Rep[Option[Int]]), (EmployeeId, Name, Option[StoreId], Option[Boolean], Option[Int])] =
      employeeRef
      .projected(_.filter(_.role =!= (Role.Manager: Role)))
      .projected{
        (q: employeeRef.Q) =>
          q.joinLeft(Products).on(_.worksAt === _.soldBy)
           .groupBy(_._1)
           .map{
             case (e, ps) =>
               val numPs: Rep[Option[Int]] = ps.map(_._2.map(_.quantity)).sum
               (e.id, e.name, e.worksAt, e.good, numPs)
           }.sortBy(_._5.desc)
      }
      .build(EditorId("employees"), EditorName("Employees by products handled"))

    val plan = new IntegrationUnfiltered(
      employeeEd, productsForEmployeeEd, productsEd, storesEd, storeNickNamesEd
    )
  }
}

object Runner extends App with LazyLogging {
  val port   = Properties.envOrElse("PORT", "8080").toInt
  val ctxOpt = Properties envOrNone "CTX" map (ctx => s"/$ctx" replaceAll("//", "/"))

  logger.info(s"Starting on port $port on ${ctxOpt getOrElse "/"}")

  val mountPlan: Server ⇒ Plan ⇒ Server =
    ctxOpt.fold[Server => Plan => Server](s => p => s plan p)(
      ctx => s => p => (s context ctx)(_ plan p)
    )

  val server: Server =
    mountPlan(unfiltered.jetty.Server.http(port))(CrudDemoWebApp.Demo.plan)

  server.run()
}
