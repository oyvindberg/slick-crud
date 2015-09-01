package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.MuiCheckbox
import com.olvind.stringifiers.{RenderHint, ValueNotInSet, ValueNotValid}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js
import scalacss.ScalaCssReact._

object TableCell {
  case class Props (
    createMode:     Boolean,
    inputEnabled:   Boolean,
    column:         ColumnDesc,
    clearError:     Callback,
    rowIdU:         U[StrRowId],
    valueU:         U[StrValue],
    cachedDataOpt:  Option[CachedData],
    onUpdateU:      U[StrValue ⇒ Callback],
    showSingleRowU: U[RouterCtl[StrRowId]],
    errorU:         U[ValidationError]){
    def tableName = column.ref.table
  }

  implicit val r0 = ComponentUpdates.InferredReusability[Props]

  case class State(contentU: U[StrValue])

  private final case class Backend($: BackendScope[Props, State]) {
    val selectedAttr = ^.selected := "selected"
    val disabledAttr = ^.disabled := "disabled"
    val checkedAttr  = ^.checked  := "checked"
    val booleanVals  = Seq(true, false).map(b ⇒ StrValue(b.toString))

    lazy val callbacks = Px.cbA($.props).map(Callbacks)

    case class Callbacks(P: Props){
      val persistEvent: ReactEvent => Callback =
        e => Callback(e.persist())

      val onEditFinishedU: U[U[StrValue] ⇒ Callback] =
        P.onUpdateU flatMap {
          onUpdate ⇒ (uv: U[StrValue]) ⇒
            uv.filterNot(_.uSome =:= P.valueU).map(onUpdate) getOrElse Callback.empty
        }

      val onCheckChangedU: U[(ReactEvent, Boolean) ⇒ Callback] =
        onEditFinishedU map {
          f ⇒ (e, b) ⇒ persistEvent(e) >> f(StrValue(b.toString))
        }

      val onSelectChangedU: U[ReactEventI ⇒ Callback] =
        onEditFinishedU.map {
          f ⇒ e ⇒ persistEvent(e) >> f(StrValue(e.target.value))
        }

      val goToRowU: U[ReactEvent ⇒ Callback] = for {
        rowId         ← P.rowIdU
        showSingleRow ← P.showSingleRowU
      } yield showSingleRow.setEH(rowId)

      val onEditCanceled: Callback =
        $.modState(_.copy(contentU = P.valueU))

      val onEdit: ReactKeyboardEventI ⇒ Callback =
        _.nativeEvent.keyCode match {
          case KeyCode.Escape ⇒ onEditCanceled
          case _              ⇒ P.clearError
        }

      val onChange: ReactKeyboardEventI ⇒ Callback =
        e ⇒ persistEvent(e) >> $.modState(_.copy(contentU = StrValue(e.target.value)))

      val restrictedValues: Option[Seq[StrValue]] =
        P.cachedDataOpt.map(_.restrictedValues).flatMap(
          _.collectFirst{
            case (P.column.ref, values) ⇒ values
          }
        )
    }

    def render(P: Props, S: State): ReactElement = {
      val cb       = callbacks.value()
      val optional = P.column.isOptional || P.createMode

      val elem: ReactElement =
        (P.column.rendering, cb.restrictedValues) match {
          case (RenderHint.Uri, _) ⇒
            (P.createMode, S.contentU.toOption) match {
              case (false, Some(content)) => link(content, cb.goToRowU)
              case _                      => normalInput(P, S, cb, RenderHint.Text)
            }

          case (RenderHint.Boolean, rs) ⇒
            if (optional)
              select(P, S, cb, isOptional = true, booleanVals)
            else
              MuiCheckbox(
                onCheck        = cb.onCheckChangedU,
                defaultChecked = S.contentU =:= StrValue("true"),
                disabled       = !P.inputEnabled
              )()

          case (_, Some(rs: Seq[StrValue])) ⇒
            select(P, S, cb, optional, rs)

          case (r, _) =>
            normalInput(P, S, cb, r)
        }

      <.div(
        TableStyle.cell,
        elem,
        P.errorU.map(validationError)
      )
    }

    def validationError(ve: ValidationError): ReactTagOf[dom.html.Div] = {
      val msg: String =
        ve match {
          case (_, Some(ValueNotValid(value, tpe, eo))) =>
            s"$value is not a valid ${tpe.value}: ${eo.getOrElse("")}"
          case (_, Some(ValueNotInSet(value, tpe, _))) ⇒
            "Choose a valid value"
          case (_, None) ⇒
            "Provide a valid value"
        }
      <.div(TableStyle.error, msg)
    }

    def select(P: Props, S: State, cb: Callbacks, isOptional: Boolean, vs: Seq[StrValue]): ReactTagOf[dom.html.Select] =
     <.select(
        ^.onChange     ==>? cb.onSelectChangedU.liftParam,
        ^.defaultValue  :=? S.contentU,
       !P.inputEnabled   ?= disabledAttr,
       isOptional        ?= <.option(^.value := ""),
        vs.map(v ⇒ <.option(v.value, ^.value := v))
     )

    def normalInput(P: Props, S: State, cb: Callbacks, r: RenderHint): ReactTagOf[dom.html.Input] =
      <.input(
        TableStyle.cell,
        !P.inputEnabled   ?= disabledAttr,
        ^.value          :=? S.contentU,
        ^.`type`          := (if (r =:= RenderHint.Int) "number" else "text"),
        ^.placeholder     := P.column.typename.value,
        ^.autoComplete    := false,
        ^.onBlur        -->? cb.onEditFinishedU.mapply(S.contentU),
        ^.onKeyDown      ==> cb.onEdit,
        ^.onChange       ==> cb.onChange
      )

    def link(label: StrValue, action: U[ReactEvent ⇒ Callback]): ReactElement =
      Button(label.value, action, Button.Secondary, small = true, enabled = action.isDefined)
  }

  private val component = ReactComponentB[Props]("TableCell")
    .initialState_P(P ⇒ State(P.valueU))
    .renderBackend[Backend]
    .componentWillReceiveProps(receiveProps ⇒
      receiveProps.$.modState(_.copy(contentU = receiveProps.nextProps.valueU))
//      .when(receiveProps.currentProps.valueU =/= receiveProps.nextProps.valueU)
//      .void
    )
    .configure(ComponentUpdates.inferred("TableCell"))
    .build

  def createMode(clearError:    Callback,
                 cachedDataOpt: Option[CachedData],
                 updateU:       StrValue ⇒ Callback,
                 col:           ColumnDesc,
                 valueU:        U[StrValue],
                 errorU:        U[ValidationError],
                 inputEnabled:  Boolean): ReactElement =

    component.withKey(col.ref.name.value)(
      Props(createMode = true, inputEnabled, col, clearError, js.undefined, valueU, cachedDataOpt, updateU, js.undefined, errorU)
    )

  def apply(clearError:     ColumnRef => Callback,
            cachedDataOpt:  Option[CachedData],
            updateU:        U[ColumnRef ⇒ StrValue ⇒ Callback],
            showSingleRowU: U[RouterCtl[StrRowId]])
           (editorDesc:     EditorDesc,
            col:            ColumnDesc,
            rowIdU:         U[StrRowId],
            valueU:         U[StrValue],
            errorU:         U[ValidationError]): ReactElement = {

    val inputEnabled: Boolean = {
      def tableMatches = col.ref.table =:= editorDesc.mainTable
      def isAllowed    = editorDesc.isEditable  && col.isEditable
      tableMatches && isAllowed && rowIdU.isDefined
    }

    component.withKey(rowIdU.map(_.value).getOrElse("") + col.ref.name.value)(
      Props(createMode = false, inputEnabled, col, clearError(col.ref), rowIdU, valueU, cachedDataOpt, updateU.liftParam(col.ref), showSingleRowU, errorU)
    )
  }
}
