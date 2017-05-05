package com.olvind.crud.demo

import scala.io.Source
import scala.util.Random

trait GenDataModule extends StoreDomain {
  object GenData {
    private def readLines(resourceName: String): Iterator[String] = {
      Source.fromInputStream(getClass.getResourceAsStream("/" + resourceName), "UTF-8")
        .getLines()
        .map(_.trim)
        .filterNot(_.isEmpty)
    }

    private def extractStores(lines: List[String]): List[Store] = lines match {
      case (name) :: (description) :: tail ⇒
        Store(
          StoreId(name.split("[^a-zA-Z0-9]").map(_.trim.toLowerCase).filterNot(_.isEmpty).mkString("").take(20)),
          Name(name),
          Some(Desc(description)).filterNot(_ ⇒ Random.nextInt(6) == 0),
          Random.nextBoolean()
        ) :: extractStores(tail)
      case _ ⇒ Nil
    }
    private def howMany = 1 + Random.nextInt(6)

    //thanks to http://grammar.about.com/od/words/a/punnamestores.htm
    val stores = extractStores(readLines("stores.txt").toList)

    //listofrandomnames.com
    val names = readLines("names.txt")

    def randomRole = Random.nextInt(3) match {
      case 0 ⇒ None
      case 1 ⇒ Some(Role.Employee)
      case 2 ⇒ Some(Role.Manager)
    }

    def randomGood = Random.nextInt(3) match {
      case 0 ⇒ None
      case 1 ⇒ Some(true)
      case 2 ⇒ Some(false)
    }

    val employees = stores.flatMap { store ⇒
      0 until howMany map { n ⇒
        Employee(EmployeeId(0), Name(names.next()), Option(store.id).filter(_ ⇒ Random.nextBoolean()), randomRole, randomGood)
      }
    }
    val products = stores.flatMap { store ⇒
      0 until howMany map { n ⇒
        Product(ProductId(0), Name("product name"), Random.nextInt(500), Price(Random.nextInt(100)), store.id)
      }
    }
    val storeNicknames: Seq[(StoreId, String)] = stores.zipWithIndex.map{case (s, idx) ⇒ (s.id, s.name.value.split("\\s+")(0) + "lly")}.toSeq
  }
}