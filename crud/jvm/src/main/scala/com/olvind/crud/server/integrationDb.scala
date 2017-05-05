package com.olvind.crud
package server

import slick.jdbc.JdbcBackend

trait integrationDb extends integrationSlick {
  def db: JdbcBackend#Database
}
