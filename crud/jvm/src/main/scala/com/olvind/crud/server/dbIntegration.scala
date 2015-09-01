package com.olvind.crud
package server

import slick.jdbc.JdbcBackend

trait dbIntegration extends slickIntegration {
  def db: JdbcBackend#Database
}
