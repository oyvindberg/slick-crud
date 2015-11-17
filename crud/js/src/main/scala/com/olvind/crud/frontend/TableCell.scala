package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.MuiCheckbox
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
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

  implicit val r0 = ComponentUpdates.InferredReusability[Props]

  case class State(contentU: U[StrValue])

  final case class Backend($: BackendScope[Props, State]) {
    val selectedAttr = ^.selected := "selected"
    val disabledAttr = ^.disabled := "disabled"
    val checkedAttr  = ^.checked  := "checked"
    val booleanVals  = Seq(true, false).map(b ⇒ StrValue(b.toString))

    lazy val fromProps = Px.cbA($.props).map(FromProps)

    case class FromProps(P: Props){

      val onEditFinishedU: U[U[StrValue] ⇒ Callback] =
        P.onUpdateU flatMap {
          onUpdate ⇒ (uv: U[StrValue]) ⇒
            uv.filterNot(_.undef =:= P.valueU).map(onUpdate).voidU
        }

      val onCheckChangedU: U[(ReactEvent, Boolean) ⇒ Callback] =
        onEditFinishedU map {
          f ⇒ (e, b) ⇒ f(StrValue(b.toString))
        }

      val onSelectChangedU: U[ReactEventI ⇒ Callback] =
        onEditFinishedU.map {
          f ⇒ e ⇒ f(StrValue(e.target.value))
        }

      val goToRowU: U[ReactEvent ⇒ Callback] = for {
        rowId         ← P.rowIdU
        showSingleRow ← P.showSingleRowU
      } yield showSingleRow.setEH(rowId)

      val onEditCanceled: Callback =
        $.modState(_.copy(contentU = P.valueU))

      val onEdit: ReactKeyboardEventI ⇒ Callback =
        _.nativeEvent.keyCode match {
          case KeyCode.Escape               ⇒ onEditCanceled
          case _                            ⇒ P.clearError
        }

      val onChange: ReactKeyboardEventI ⇒ Callback =
        e ⇒ $.modState(_.copy(contentU = StrValue(e.target.value)))

      val restrictedValues: Option[Seq[StrValue]] =
        P.cachedDataU.toOption.map(_.restrictedValues).flatMap(
          _.collectFirst{
            case (P.column.ref, values) ⇒ values
          }
        )
    }

    def render(P: Props, S: State): ReactElement = {
      val cb       = fromProps.value()
      val optional = P.column.isOptional || P.createMode

      val elem: ReactElement = (P.column.rendering, cb.restrictedValues) match {
        case (CellRendering.Link, _) ⇒
          if (P.createMode)
            normalInput(P, S, CellRendering.Text)
          else link(S)

        case (CellRendering.Checkbox, rs) ⇒
          if (optional)
            select(P, S, isOptional = true, booleanVals)
          else
            MuiCheckbox(
              onCheck        = cb.onCheckChangedU,
              defaultChecked = S.contentU =:= StrValue("true"),
              disabled       = !P.inputEnabled
            )()

        case (_, Some(rs: Seq[StrValue])) ⇒
          select(P, S, optional, rs)

        case (r, _) =>
          normalInput(P, S, r)
      }

      <.div(
        TableStyle.cell,
        elem,
        P.errorU.map(
          e ⇒ <.div(TableStyle.error, e.value)
        )
      )
    }

    def select(P: Props, S: State, isOptional: Boolean, vs: Seq[StrValue]) =
     <.select(
        ^.onChange           ==>? fromProps.value().onSelectChangedU.liftParam,
        ^.defaultValue        :=? S.contentU,
       !P.inputEnabled         ?= disabledAttr,
       isOptional              ?= <.option(^.value := ""),
        vs.map(v ⇒ <.option(v.value, ^.value := v))
     )

    def normalInput(P: Props, S: State, r: CellRendering) =
      <.input(
        TableStyle.cell,
        !P.inputEnabled ?= disabledAttr,
        ^.value              :=? S.contentU,
        ^.onBlur            -->? fromProps.value().onEditFinishedU.mapply(S.contentU),
        ^.`type`              := (if (r =:= CellRendering.Number) "number" else "text"),
        ^.placeholder         := P.column.typeName,
        ^.autoComplete        := false,
        ^.onKeyDown          ==> fromProps.value().onEdit,
        ^.onChange           ==> fromProps.value().onChange
      )

    def link(S: State) =
      Button(
        S.contentU.map(_.value),
        fromProps.value().goToRowU,
        Button.Secondary,
        small   = true,
        enabled = fromProps.value().goToRowU.isDefined
      )
  }

  private val component = ReactComponentB[Props]("TableCell")
    .initialState_P(P ⇒ State(P.valueU))
    .renderBackend[Backend]
    .componentWillReceiveProps(receiveProps ⇒
      receiveProps.$.modState(_.copy(contentU = receiveProps.nextProps.valueU))
//        .conditionally(receiveProps.currentProps.valueU =/= receiveProps.nextProps.valueU)
//        .void
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
