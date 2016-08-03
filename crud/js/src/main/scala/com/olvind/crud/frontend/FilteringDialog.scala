package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.{MuiDialog, MuiDropDownMenu, MuiMenuItem}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Px, Reusability, ReusableFn}
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js
import scalacss.ScalaCssReact._

object FilteringDialog {
  final case class Props(
    sendAction:    ReusableFn[Action, Callback],
    editorId:      EditorId,
    cols:          Seq[ColumnDesc],
    initial:       Option[Filter],
    cachedDataOpt: Option[CachedData],
    dialogOpen:    Boolean,
    closeDialog:   Callback
  )

  final case class State(
    chosenColumn: ColumnDesc,
    valueOpt:     Option[StrValue]
  )

  object State {
    def apply(P: Props, of: Option[Filter]): State =
      State(
        of flatMap (f ⇒ P.cols.find(_.ref =:= f.columnInfo)) getOrElse P.cols.head,
        of.map(_.value)
      )
  }

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.byRef[Props]
  private implicit val ReusableState: Reusability[State] =
    Reusability.byRef[State]

  private final case class Backend($: BackendScope[Props, State]){

    lazy val fromProps: Px[FromProps] =
      Px.cbA($.props).map(FromProps)

    case class FromProps(P: Props) {
      val renderedCols: js.Array[ReactComponentU_] =
        P.cols.map(col ⇒ MuiMenuItem(key = col.ref.toString, value = col, primaryText = col.name.value)()).toJsArray

      val onChangedCol: (ReactEvent, Int, ColumnDesc) ⇒ Callback =
        (e, idx, col) ⇒ $.modState(_.copy(chosenColumn = col, valueOpt = None))

      def renderedDropdown(chosenColumn: ColumnDesc): ReactElement =
        MuiDropDownMenu(value = chosenColumn, onChange = onChangedCol)(renderedCols)

      val onParamsChanged: Option[Filter] ⇒ Callback =
        of ⇒ P.sendAction(FetchFilteredData(P.editorId, of))

      def onApply(c: ColumnDesc)(text: StrValue): ReactEvent ⇒ Callback =
        _ ⇒ P.closeDialog >> onParamsChanged(Filter(c.ref, text).some)

      val onClear: ReactEvent ⇒ Callback =
        _ ⇒ P.closeDialog >> onParamsChanged(None)

      val clearBtn: ReactElement =
        Button(
          labelOpt   = "Clear filter",
          onClickOpt = Some(onClear),
          tpe        = Button.Normal
        )
    }

    val onTextChange: StrValue ⇒ Callback =
      v ⇒ $.modState(_.copy(valueOpt = Some(v)))

    def render(P: Props, S: State): ReactElement = {
      val fp = fromProps.value()

      val applyBtn: ReactElement =
        Button(
          labelOpt   = "Apply filter",
          onClickOpt = S.valueOpt.map(fp.onApply(S.chosenColumn)),
          tpe        = Button.Primary,
          enabled    = S.valueOpt.isDefined
        )

      MuiDialog(title   = "Filtering",
                actions = js.Array(fp.clearBtn, applyBtn),
                open    = P.dialogOpen)(
        <.div(
          TableStyle.row,
          fp.renderedDropdown(S.chosenColumn),
          TableCell.createMode(
            sendAction         = P.sendAction,
            cachedDataOpt      = P.cachedDataOpt,
            editorId           = P.editorId,
            col                = S.chosenColumn,
            valueOpt           = S.valueOpt,
            validationErrorOpt = None,
            inputEnabled       = true,
            onUpdateOpt        = Some(onTextChange)
          )
        )
      )
    }
  }

  private val component =
    ReactComponentB[Props]("Filtering")
      .initialState_P(P ⇒ State(P, P.initial))
      .renderBackend[Backend]
      .configure(ShouldUpdate.apply)
      .componentWillReceiveProps(receiveProps ⇒
        receiveProps.$.setState(State(receiveProps.nextProps, receiveProps.nextProps.initial))
      ).build

  def apply(p: Props): ReactElement =
    component(p)
}