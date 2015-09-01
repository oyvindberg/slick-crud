package com.olvind.crud
package server

import slick.driver.JdbcDriver

trait integrationSlick {
  val driver: JdbcDriver
}
