package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.{MuiDialog, MuiDropDownMenu, MuiMenuItem}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js
import scalacss.ScalaCssReact._

object FilteringDialog {
  case class Props(
    cols:           Seq[ColumnDesc],
    initial:        Option[Filter],
    onParamsChange: Option[Filter] ⇒ Callback,
    cachedDataOpt:  Option[CachedData],
    dialogOpen:     Boolean,
    closeDialog:    Callback
  )

  implicit val r0 = ComponentUpdates.InferredReusability[Props]

  case class State(
    chosenColumn: ColumnDesc,
    valueU:       U[StrValue]
  )

  object State {
    def apply(P: Props, of: Option[Filter]): State =
      State(
        of flatMap (f ⇒ P.cols.find(_.ref =:= f.columnInfo)) getOrElse P.cols.head,
        of.map(_.value).asUndef
      )
  }

  private final case class Backend($: BackendScope[Props, State]){

    lazy val fromProps: Px[FromProps] =
      Px.cbA($.props).map(FromProps)

    case class FromProps(P: Props) {
      val renderedCols: js.Array[ReactComponentU_] =
        P.cols.map(col ⇒ MuiMenuItem(key = col.ref.toString, value = col, primaryText = col.name.value)()).toJsArray

      val onChangedCol: (ReactEvent, Int, ColumnDesc) ⇒ Callback =
        (e, idx, col) ⇒ $.modState(_.copy(chosenColumn = col, valueU = js.undefined))

      def renderedDropdown(chosenColumn: ColumnDesc): ReactElement =
        MuiDropDownMenu(value = chosenColumn, onChange = onChangedCol)(renderedCols)

      def onApply(c: ColumnDesc)(text: StrValue): ReactEvent ⇒ Callback =
        _ ⇒ P.closeDialog >> P.onParamsChange(Filter(c.ref, text).some)

      val onClear: ReactEvent ⇒ Callback =
        _ ⇒ P.closeDialog >> P.onParamsChange(None)

      val clearBtn: ReactElement =
        Button(
          labelU   = "Clear filter",
          onClickU = onClear,
          tpe      = Button.Normal
        )
    }

    val onTextChange: StrValue ⇒ Callback =
      v ⇒ $.modState(_.copy(valueU = v))

    def render(P: Props, S: State): ReactElement = {
      val fp = fromProps.value()

      val applyBtn: ReactElement =
        Button(
          labelU    = "Apply filter",
          onClickU  = S.valueU.map(fp.onApply(S.chosenColumn)),
          tpe       = Button.Primary,
          enabled   = S.valueU.isDefined
        )

      MuiDialog(title   = "Filtering",
                actions = js.Array(fp.clearBtn, applyBtn),
                open    = P.dialogOpen)(
        <.div(
          TableStyle.row,
          fp.renderedDropdown(S.chosenColumn),
          TableCell.createMode(
            cachedDataOpt = P.cachedDataOpt,
            updateU       = onTextChange,
            col           = S.chosenColumn,
            clearError    = Callback.empty,
            valueU        = S.valueU,
            errorU        = js.undefined,
            inputEnabled  = true
          )
        )
      )
    }
  }

  private val component =
    ReactComponentB[Props]("Filtering")
      .initialState_P(P ⇒ State(P, P.initial))
      .renderBackend[Backend]
      .configure(ComponentUpdates.inferred("Filtering"))
      .componentWillReceiveProps(receiveProps ⇒
        receiveProps.$.setState(State(receiveProps.nextProps, receiveProps.nextProps.initial))
      ).build

  def apply(p: Props): ReactElement =
    component(p)
}