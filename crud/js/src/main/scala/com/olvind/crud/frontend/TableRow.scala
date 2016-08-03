package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Px, Reusability, ReusableFn}
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object TableRow {
  final case class Props(
    sendAction:      ReusableFn[Action, Callback],
    editor:          EditorDesc,
    row:             StrTableRow,
    cachedDataOpt:   Option[CachedData],
    validationFails: Option[Map[ColumnRef, ValidationError]]
  )

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.caseClass[Props]

  private final case class Backend($: BackendScope[Props, Unit]){
    val cachedRendered: Px[Seq[ReactElement]] =
      Px.cbA($.props) map {
        P ⇒
          P.row.values.zip(P.editor.columns).map{
            case (value: StrValue, col: ColumnDesc) ⇒
              TableCell(
                sendAction         = P.sendAction,
                editorDesc         = P.editor,
                col                = col,
                rowIdOpt           = P.row.idOpt,
                cachedDataOpt      = P.cachedDataOpt,
                valueOpt           = Some(value),
                validationErrorOpt = P.validationFails.flatMap(_.get(col.ref)),
                onUpdateOpt        = P.row.idOpt.map(
                  rowId ⇒ (value: StrValue) ⇒
                    P.sendAction(UpdateValue(P.editor.editorId, rowId, col.ref, value))
                )
              )
          }
      }

    def render(P: Props): ReactElement = {
      <.div(
        TableStyle.row,
        cachedRendered.value()
      )
    }
  }

  private val component =
    ReactComponentB[Props]("TableRow")
      .renderBackend[Backend]
      .configure(ShouldUpdate.apply)
      .build

  def apply(p: Props): ReactElement =
    component.withKey(p.row.idOpt.fold("")(_.value))(p)
}