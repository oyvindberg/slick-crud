package com.olvind.crud
package frontend

import japgolly.scalajs.react.extra.{LogLifecycle, Reusability}
import japgolly.scalajs.react.{ReactComponentB, TopNode}

object ShouldUpdate {
  private val enableDebug = false

  private def go[P: Reusability, S: Reusability, B, N <: TopNode]
           (forceDebug: Boolean) =

    (c: ReactComponentB[P, S, B, N]) ⇒
      if (enableDebug || forceDebug)
        c.configure(Reusability.shouldComponentUpdateWithOverlay)
          .configure(Reusability.shouldComponentUpdateAndLog(c.name))
          .configure(LogLifecycle.short)
      else
        c.configure(Reusability.shouldComponentUpdate)

  def forceDebug[P: Reusability, S: Reusability, B, N <: TopNode]: (ReactComponentB[P, S, B, N]) => ReactComponentB[P, S, B, N] =
    go[P, S, B, N](forceDebug = true)

  def apply[P: Reusability, S: Reusability, B, N <: TopNode]: (ReactComponentB[P, S, B, N]) => ReactComponentB[P, S, B, N] =
    go[P, S, B, N](forceDebug = false)
}

