package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.MuiSnackbar
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js
import scala.concurrent.duration._

object UpdateNotifierClient {

  val ttl = 3.seconds

  case class Props(res: List[TimedRes])

  private final class Backend($: BackendScope[Props, Unit]) {
    val onRequestClose: TimedRes => String => Callback =
      t => reason => Callback.info(s"deleting $t because $reason")

    def render(P: Props) = {
      val elems: List[ReactElement] =
        P.res.zipWithIndex map {
          case (t@TimedT(started, d, (coord, XSuccess(_), None)), idx) ⇒
            <.div(): ReactElement

          case (t@TimedT(started, d, (coord, XSuccess(_), Some(msg))), idx) ⇒
            /* css hack so we can display several, and with more text*/
            val bottom = js.Dynamic.literal(
              bottom   = s"${24 + idx * 55 }px",
              maxWidth = "750px"
            )
            MuiSnackbar(
              open           = true,
              onRequestClose = onRequestClose(t),
              message        = msg + s" (${d.toMillis} ms))",
              style          = bottom
            )()

          case (t@TimedT(started, d, (coord, f: XFail, _)), idx) ⇒
            val bottom: js.Any = js.Dynamic.literal(bottom = s"${24 + idx * 55 }px")
            MuiSnackbar(
              open           = true,
              message        = f.toString, //todo:toString
              style          = bottom,
              onRequestClose = onRequestClose(t)
            )()
        }
      <.div(elems)
    }
  }

  private val component = ReactComponentB[Props]("UpdateNotifierClient")
    .renderBackend[Backend]
    .build

  def apply(res: List[TimedRes]): ReactElement =
    component(Props(res))
}
