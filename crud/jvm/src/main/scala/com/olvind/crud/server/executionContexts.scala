package com.olvind.crud.server

import scala.concurrent.ExecutionContextExecutor

trait executionContexts {
  implicit def executionContext: ExecutionContextExecutor
}
