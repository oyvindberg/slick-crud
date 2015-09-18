package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.MuiCheckBox
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode

import scalacss.ScalaCssReact._

object TableCell {
  case class Props (
    createMode:     Boolean,
    inputEnabled:   Boolean,
    column:         ColumnDesc,
    clearError:     Callback,
    rowIdU:         U[StrRowId],
    valueU:         U[StrValue],
    cachedDataU:    U[CachedData],
    onUpdateU:      U[StrValue ⇒ Callback],
    showSingleRowU: U[RouterCtl[StrRowId]],
    errorU:         U[ErrorMsg]){
    def tableName = column.ref.table
  }

  case class State(contentU: U[StrValue])

  final case class Backend($: WrapBackendScope[Props, State]) {
    val selectedAttr = ^.selected := "selected"
    val disabledAttr = ^.disabled := "disabled"
    val checkedAttr  = ^.checked  := "checked"
    val booleanVals  = Seq(true, false).map(b ⇒ StrValue(b.toString))

    val onEditFinishedU: U[U[StrValue] ⇒ Callback] =
      $.props.onUpdateU flatMap {
        onUpdate ⇒ (uv: U[StrValue]) ⇒
          uv.filterNot(_.undef =:= $.props.valueU).map(onUpdate).voidU
      }

    val onCheckChangedU: U[(ReactEventI, Boolean) ⇒ Callback] =
      onEditFinishedU map {
        f ⇒ (e, b) ⇒ f(StrValue(b.toString))
      }

    val onSelectChangedU: U[ReactEventI ⇒ Callback] =
      onEditFinishedU.map {
        f ⇒ e ⇒ f(StrValue(e.target.value))
      }

    val goToRowU: U[ReactEvent ⇒ Callback] = for {
      rowId         ← $.props.rowIdU
      showSingleRow ← $.props.showSingleRowU
    } yield showSingleRow.setEH(rowId)

    val onEditCanceled: Callback =
      $.modState(_.copy(contentU = $.props.valueU))

    val onEdit: ReactKeyboardEventI ⇒ Callback =
      _.nativeEvent.keyCode match {
        case KeyCode.Escape               ⇒ onEditCanceled
        case _                            ⇒ $.props.clearError
      }

    val onChange: ReactKeyboardEventI ⇒ Callback =
      e ⇒ $.modState(_.copy(contentU = StrValue(e.target.value)))

    def render(S: State): ReactElement = {
      val c = $.props.column
      val ors: Option[Seq[StrValue]] =
        $.props.cachedDataU.toOption.map(_.restrictedValues).flatMap(
          _.collectFirst{
            case (c.`ref`, values) ⇒ values
          }
        )
      val optional = c.isOptional || $.props.createMode

      val elem: ReactElement = (c.rendering, ors) match {
        case (CellRendering.Link, _) ⇒
          if ($.props.createMode)
            normalInput(S, CellRendering.Text)
          else link(S)

        case (CellRendering.Checkbox, _) ⇒
          if (optional)
            select(S, isOptional = true, booleanVals)
          else
            MuiCheckBox(
              onCheck        = onCheckChangedU,
              defaultChecked = S.contentU =:= StrValue("true"),
              disabled       = !$.props.inputEnabled
            )

        case (_, Some(rs: Seq[StrValue])) ⇒
          select(S, optional, rs)

        case (r, _) =>
          normalInput(S, r)
      }

      <.div(
        TableStyle.cell,
        elem,
        $.props.errorU.map(
          e ⇒ <.div(TableStyle.error, e.value)
        )
      )
    }

    def select(S: State, isOptional: Boolean, vs: Seq[StrValue]) =
     <.select(
        ^.onChange           ==>? onSelectChangedU.liftParam,
        ^.defaultValue        :=? S.contentU,
       !$.props.inputEnabled   ?= disabledAttr,
       isOptional              ?= <.option(^.value := ""),
        vs.map(v ⇒ <.option(v.value, ^.value := v))
     )

    def normalInput(S: State, r: CellRendering) =
      <.input(
        TableStyle.cell,
        !$.props.inputEnabled ?= disabledAttr,
        ^.value              :=? S.contentU,
        ^.onBlur            -->? onEditFinishedU.mapply(S.contentU),
        ^.`type`              := (if (r =:= CellRendering.Number) "number" else "text"),
        ^.placeholder         := $.props.column.typeName,
        ^.autoComplete        := false,
        ^.onKeyDown          ==> onEdit,
        ^.onChange           ==> onChange
      )

    def link(S: State) =
      Button(
        S.contentU.map(_.value),
        goToRowU,
        Button.Secondary,
        small   = true,
        enabled = goToRowU.isDefined
      )
  }

  private val component = ReactComponentB[Props]("TableCell")
    .initialState_P(P ⇒ State(P.valueU))
    .backend($ ⇒ Backend(WrapBackendScope($)))
    .render($ ⇒ $.backend.render($.state))
    .componentWillReceiveProps(($, P) ⇒
      $.modState(_.copy(contentU = P.valueU))
        .conditionally(P.valueU =/= $.props.valueU)
        .void
    )
    .configure(ComponentUpdates.inferred("TableCell"))
    .build

  def createMode(clearError:     Callback,
                 cachedDataU:    U[CachedData],
                 updateU:        StrValue ⇒ Callback,
                 col:            ColumnDesc,
                 valueU:         U[StrValue],
                 errorU:         U[ErrorMsg],
                 inputEnabled:   Boolean) = {
    component.withKey(col.ref.name.value)(
      Props(createMode = true, inputEnabled, col, clearError, uNone, valueU, cachedDataU, updateU, uNone, errorU)
    )
  }

  def apply(clearError:     ColumnRef => Callback,
            cachedDataU:    U[CachedData],
            updateU:        U[ColumnRef ⇒ StrValue ⇒ Callback],
            showSingleRowU: U[RouterCtl[StrRowId]])
           (editorDesc:     EditorDesc,
            col:            ColumnDesc,
            rowIdU:         U[StrRowId],
            valueU:         U[StrValue],
            errorU:         U[ErrorMsg]) = {

    val inputEnabled: Boolean = {
      def tableMatches = col.ref.table =:= editorDesc.mainTable
      def isAllowed    = editorDesc.isEditable  && col.isEditable
      tableMatches && isAllowed && rowIdU.isDefined
    }

    component.withKey(rowIdU.map(_.value).getOrElse("") + col.ref.name.value)(
      Props(createMode = false, inputEnabled, col, clearError(col.ref), rowIdU, valueU, cachedDataU, updateU.liftParam(col.ref), showSingleRowU, errorU)
    )
  }
}
