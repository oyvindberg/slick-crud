package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.MuiRaisedButton
import japgolly.scalajs.react._

import scala.scalajs.js

object Button {
  case class Props(
    label:    U[String],
    onClickU: U[ReactEvent ⇒ Callback],
    tpe:      ButtonType,
    enabled:  Boolean,
    small:    Boolean
  )

  sealed trait ButtonType
  case object Normal    extends ButtonType
  case object Primary   extends ButtonType
  case object Secondary extends ButtonType

  private val smallLabelU: U[js.Any] =
    js.Dynamic.literal(fontSize = "smaller")

  private val component = ReactComponentB[Props]("Button")
    .render_P(P ⇒
      MuiRaisedButton(
        label       = P.label,
        onMouseUp   = P.onClickU,
        primary     = P.tpe =:= Primary,
        secondary   = P.tpe =:= Secondary,
        labelStyle  = smallLabelU.filter(_ ⇒ P.small),
        disabled    = !P.enabled || P.onClickU.isEmpty
      )()
    ).build

  def apply(labelU:   U[String],
            onClickU: U[ReactEvent ⇒ Callback],
            tpe:      ButtonType,
            enabled:  Boolean = true,
            small:    Boolean = false) =
    component(Props(labelU, onClickU, tpe, enabled, small))
}
