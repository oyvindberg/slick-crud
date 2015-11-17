package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.{MuiDropDownMenuItem => Item, _}
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
    cachedDataU:    U[CachedData],
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

  final case class Backend($: BackendScope[Props, State]){

    lazy val fromProps = Px.cbA($.props).map(FromProps)

    case class FromProps(P: Props) {
      object cols {
        def idx(needle: String, haystack: js.Array[Item]): U[Int] =
          haystack.indexWhere(_.payload =:= needle).undef.filter(_ =/= -1)

        val items: js.Array[Item] =
          P.cols.map(ci ⇒ Item(payload = ci.name.value, text = ci.name.value)).toJsArray

        val onSelect: (ReactEvent, Int, js.Any) ⇒ Callback =
          (e, idx, i) ⇒ $.modState(_.copy(chosenColumn = P.cols(idx), valueU = uNone))

        def dropdown(S: State): ReactElement =
          MuiDropDownMenu(
            menuItems     = cols.items,
            selectedIndex = idx(S.chosenColumn.name.value, cols.items),
            onChange      = cols.onSelect
          )()
      }

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
          fromProps.value().cols.dropdown(S),
          TableCell.createMode(
            cachedDataU  = P.cachedDataU,
            updateU      = onTextChange,
            col          = S.chosenColumn,
            clearError   = Callback.empty,
            valueU       = S.valueU,
            errorU       = uNone,
            inputEnabled = true
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

  def apply() =
    component

}