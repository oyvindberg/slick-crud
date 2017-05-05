package com.olvind.crud.server

import upickle.default._

trait integrationUpickle {
  object Autowire extends autowire.Server[String, Reader, Writer] {
    def read[ Result: Reader](p: String) = upickle.default.read[Result](p)
    def write[Result: Writer](r: Result) = upickle.default.write[Result](r)
  }
}
