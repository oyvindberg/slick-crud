package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js
import scalacss.ScalaCssReact._

object FilteringDialog {
  import MuiDropdownMenu.Item

  case class Props(
    ctl:            DialogController,
    cols:           Seq[ClientColumn],
    initial:        Option[Filter],
    onParamsChange: Option[Filter] ⇒ Callback,
    cachedDataU:    U[CachedData]                    
  )

  case class State(
    chosenColumn: ClientColumn,
    valueU:       U[StrValue]
  )

  object State {
    def apply(P: Props, of: Option[Filter]): State =
      State(
        of.flatMap(f ⇒ P.cols.find(_.column =:= f.columnInfo)).getOrElse(P.cols.head),
        of.map(_.value).asUndef
      )
  }

  class DialogController{
    private var dialogRef: U[MuiDialogM] = uNone

    def setRef(d: MuiDialogM): Unit =
      dialogRef = d

    def invalidate(): Callback =
      Callback(dialogRef = uNone)

    val openDialog: Callback =
      Callback(dialogRef.foreach(_.show()))

    val closeDialog: Callback =
      Callback(dialogRef.foreach(_.dismiss()))
  }

  final case class Backend($: WrapBackendScope[Props, State]){
    def idx(needle: String, haystack: js.Array[Item]): U[Int] =
      haystack.indexWhere(_.payload =:= needle).undef.filter(_ =/= -1)

    object cols {
      val items: js.Array[Item] =
        $.props.cols.map(ci ⇒ Item(payload = ci.name.value, text = ci.name.value)).toJsArray

      val onSelect: (ReactEvent, Int, Item) ⇒ Callback =
        (e, idx, i) ⇒ $.modState(_.copy(chosenColumn = $.props.cols(idx), valueU = uNone))

      def dropdown(S: State): ReactElement =
        MuiDropdownMenu(
          menuItems     = cols.items,
          selectedIndex = idx(S.chosenColumn.name.value, cols.items),
          onChange      = cols.onSelect
        )
    }

    val onTextChange: StrValue ⇒ Callback =
      v ⇒ $.modState(_.copy(valueU = v))

    def onApply(c: ClientColumn)(text: StrValue): ReactEvent ⇒ Callback =
      _ ⇒ $.props.onParamsChange(Filter(c.column, text).some) >> $.props.ctl.closeDialog

    def onClear: ReactEvent ⇒ Callback =
      _ ⇒ $.props.onParamsChange(None) >> $.props.ctl.closeDialog

    def render(S: State): ReactElement = {
      val clearBtn: ReactElement =
        Button(
          labelU   = "Clear filter",
          onClickU = onClear,
          tpe      = Button.Normal
        )

      val applyBtn: ReactElement =
        Button(
          labelU    = "Apply filter",
          onClickU  = S.valueU.map(onApply(S.chosenColumn)),
          tpe       = Button.Primary,
          enabled   = S.valueU.isDefined
        )

      MuiDialog(title   = "Filtering",
                actions = js.Array(clearBtn, applyBtn),
                ref     = $.props.ctl.setRef _)(
        <.div(
          TableStyle.row,
          cols.dropdown(S),
          TableCell.createMode(
            cachedDataU  = $.props.cachedDataU,
            updateU      = onTextChange,
            col          = S.chosenColumn,
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
    .backend($ ⇒ Backend(WrapBackendScope($)))
    .render($ => $.backend.render($.state))
    .configure(ComponentUpdates.inferred("Filtering"))
    .componentWillReceiveProps(($, P) ⇒ $.setState(State(P, P.initial)))
    .componentWillUnmount($ ⇒ $.props.ctl.invalidate())
    .build

  def apply() =
    component

}