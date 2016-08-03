package com.olvind.crud.frontend

import chandu0101.scalajs.react.components.materialui.{DeterminateIndeterminate, MuiCircularProgress}
import com.olvind.crud.StrTableRow
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB, ReactElement}

import scalacss.ScalaCssReact._

trait Waiting[T] {
  final case class Props(e: Option[T], render: T ⇒ ReactElement)

  val renderWaiting: ReactElement =
    MuiCircularProgress(
      size = 2.0,
      mode = DeterminateIndeterminate.indeterminate
    )()

  private final case class Backend($: BackendScope[Props, Unit]) {
    def render(P: Props): ReactElement = {
      val content: ReactElement = P match {
        case Props(None, _)         ⇒ renderWaiting
        case Props(Some(t), render) ⇒ render(t)
      }
      <.div(
        TableStyle.centered,
        content
      )
    }
  }

  private val component =
    ReactComponentB[Props]("Waiting")
    .stateless
    .renderBackend[Backend]
    .build

  def apply(ot: Option[T])(render: T ⇒ ReactElement): ReactElement =
    component(Props(ot, render))
}

object WaitingRows extends Waiting[Seq[StrTableRow]]
object WaitingRow extends Waiting[StrTableRow]
