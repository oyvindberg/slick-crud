package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.MuiSnackBar
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js

object UpdateNotifierClient {

  val ttl = 3000.0

  case class Props(res: List[TimedT[CrudResult]])

  private val component = ReactComponentB[Props]("UpdateNotifierClient")
    .render_P {
      P ⇒
        val elems: List[ReactElement] = P.res.zipWithIndex map {
          case (TimedT(started, d, s: CrudSuccess), idx) ⇒
            /* css hack so we can display several, and with more text*/
            val bottom: js.Any = js.Dynamic.literal(bottom = s"${24 + idx * 55 }px", maxWidth="750px")
            MuiSnackBar(
              openOnMount = true,
              message     = s.formatted + s" (${d.toMillis} ms))",
              style       = bottom
            )

          case (TimedT(started, d, f: CrudFailure), idx) ⇒
            val bottom: js.Any = js.Dynamic.literal(bottom = s"${24 + idx * 55 }px")
            MuiSnackBar(openOnMount = true, message = f.formatted, style = bottom)
        }
        <.div(elems)
    }.build

  def apply(res: List[TimedT[CrudResult]]) =
    component(Props(res))

}
