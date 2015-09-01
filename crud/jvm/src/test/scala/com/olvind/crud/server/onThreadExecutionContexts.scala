package com.olvind.crud.server

import java.util.concurrent.Executor

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

trait onThreadExecutionContexts extends executionContexts {
  final override implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(new Executor {
      def execute(task: Runnable) = task.run()
    })
}
