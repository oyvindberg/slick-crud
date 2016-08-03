package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.MuiRaisedButton
import japgolly.scalajs.react._

import scala.scalajs.js

object Button {
  final case class Props(
    label:      String,
    onClickOpt: Option[ReactEvent ⇒ Callback],
    tpe:        ButtonType,
    enabled:    Boolean,
    small:      Boolean
  )

  sealed trait ButtonType
  case object Normal    extends ButtonType
  case object Primary   extends ButtonType
  case object Secondary extends ButtonType

  private val smallLabel: js.Any =
    js.Dynamic.literal(fontSize = "smaller")

  private val component =
    ReactComponentB[Props]("Button")
      .render_P(P ⇒
        MuiRaisedButton(
          label      = P.label,
          onTouchTap = P.onClickOpt.asUndef,
          primary    = P.tpe =:= Primary,
          secondary  = P.tpe =:= Secondary,
          labelStyle = if (P.small) smallLabel else js.undefined,
          disabled   = !P.enabled || P.onClickOpt.isEmpty
        )()
      ).build

  def apply(labelOpt:   String,
            onClickOpt: Option[ReactEvent ⇒ Callback],
            tpe:        ButtonType,
            enabled:    Boolean = true,
            small:      Boolean = false): ReactElement =
    component.withKey(labelOpt)(Props(labelOpt, onClickOpt, tpe, enabled, small))
}
