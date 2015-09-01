package com.olvind.crud
package server

import slick.driver.JdbcDriver

trait slickIntegration {
  val driver: JdbcDriver
}
