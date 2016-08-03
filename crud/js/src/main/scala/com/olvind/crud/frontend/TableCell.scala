package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.MuiCheckbox
import com.olvind.stringifiers.{RenderHint, ValueNotInSet, ValueNotValid}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Px, Reusability, ReusableFn}
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode

import scalacss.ScalaCssReact._

object TableCell {
  final case class Props (
    sendAction:         ReusableFn[Action, Callback],
    createMode:         Boolean,
    inputEnabled:       Boolean,
    editorId:           EditorId,
    rowIdOpt:           Option[StrRowId],
    column:             ColumnDesc,
    valueOpt:           Option[StrValue],
    cachedDataOpt:      Option[CachedData],
    validationErrorOpt: Option[ValidationError],
    onUpdateOpt:        Option[StrValue ⇒ Callback])

  final case class State(contentOpt: Option[StrValue])

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.caseClass[Props]

  private implicit val ReusableState: Reusability[State] =
    Reusability.caseClass[State]

  private final case class Backend($: BackendScope[Props, State]) {
    val selectedAttr = ^.selected := "selected"
    val disabledAttr = ^.disabled := "disabled"
    val checkedAttr  = ^.checked  := "checked"
    val booleanVals  = Seq(true, false).map(b ⇒ StrValue(b.toString))

    lazy val callbacks: Px[Callbacks] =
      Px.cbA($.props).map(Callbacks)

    case class Callbacks(P: Props){
      val persistEvent: ReactEvent => Callback =
        e => Callback(e.persist())

      val onEditFinishedOpt: Option[Option[StrValue] ⇒ Callback] =
        P.onUpdateOpt map {
          (onUpdate: StrValue => Callback) ⇒
            (valueOpt: Option[StrValue]) ⇒
              valueOpt
                .filterNot(P.valueOpt.contains)
                .fold(Callback.empty)(onUpdate)
        }

      val onCheckChangedOpt: Option[(ReactEvent, Boolean) ⇒ Callback] =
        onEditFinishedOpt map {
          cb ⇒ (e, b) ⇒ persistEvent(e) >> cb(Some(StrValue(b.toString)))
        }

      val onSelectChangedOpt: Option[ReactEventI ⇒ Callback] =
        onEditFinishedOpt.map {
          cb ⇒ e ⇒ persistEvent(e) >> cb(Some(StrValue(e.target.value)))
        }

      val onChange: ReactKeyboardEventI ⇒ Callback =
        e ⇒ persistEvent(e) >> $.modState(_.copy(contentOpt = Some(StrValue(e.target.value))))

      val goToRowOpt: Option[ReactEvent ⇒ Callback] = for {
        rowId ← P.rowIdOpt
      } yield (e: ReactEvent) ⇒ P.sendAction(Navigate(RouteEditorRow(P.editorId, rowId)))

      val onEditCanceled: Callback =
        $.modState(_.copy(contentOpt = P.valueOpt))

      val onEdit: ReactKeyboardEventI ⇒ Callback =
        _.nativeEvent.keyCode match {
          case KeyCode.Escape ⇒ onEditCanceled
          case _              ⇒ P.sendAction(ClearValidationError(P.editorId, P.rowIdOpt, P.column.ref.some))
        }

      val restrictedValues: Option[Seq[StrValue]] =
        P.cachedDataOpt.flatMap(_.restrictedValues.get(P.column.ref))
    }

    def render(P: Props, S: State): ReactElement = {
      val cb       = callbacks.value()
      val optional = P.column.isOptional || P.createMode

      val elem: ReactElement =
        (P.column.rendering, cb.restrictedValues) match {
          case (RenderHint.Uri, _) ⇒
            (P.createMode, S.contentOpt) match {
              case (false, Some(content)) => link(content, cb.goToRowOpt)
              case _                      => normalInput(P, S, cb, RenderHint.Text)
            }

          case (RenderHint.Boolean, rs) ⇒
            if (optional)
              select(P, S, cb, isOptional = true, booleanVals)
            else
              MuiCheckbox[Nothing](
                onCheck        = cb.onCheckChangedOpt.asUndef,
                defaultChecked = S.contentOpt.contains(StrValue("true")),
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
        P.validationErrorOpt.map(validationError)
      )
    }

    def validationError(ve: ValidationError): ReactElement = {
      val msg: String =
        ve match {
          case Some(ValueNotValid(value, tpe, eo)) =>
            s"$value is not a valid ${tpe.value}: ${eo.getOrElse("")}"
          case Some(ValueNotInSet(value, tpe, _)) ⇒
            "Choose a valid value"
          case None ⇒
            "Provide a value"
        }
      <.div(TableStyle.error, msg)
    }

    def select(P: Props, S: State, cb: Callbacks, isOptional: Boolean, vs: Seq[StrValue]): ReactTagOf[dom.html.Select] =
     <.select(
       cb.onSelectChangedOpt.map(
         onSelectChanged ⇒ ^.onChange ==> onSelectChanged
       ),
        ^.defaultValue  :=? S.contentOpt,
       !P.inputEnabled   ?= disabledAttr,
       isOptional        ?= <.option(^.value := ""),
        vs.map(v ⇒ <.option(v.value, ^.value := v))
     )

    def normalInput(P: Props, S: State, cb: Callbacks, r: RenderHint): ReactTagOf[dom.html.Input] =
      <.input(
        TableStyle.cell,
        !P.inputEnabled   ?= disabledAttr,
        ^.value          :=? S.contentOpt,
        ^.`type`          := (if (r =:= RenderHint.Int) "number" else "text"),
        ^.placeholder     := P.column.typename.value,
        ^.autoComplete    := false,
        cb.onEditFinishedOpt.map(
          onEditFinished ⇒ ^.onBlur --> onEditFinished(S.contentOpt)
        ),
        ^.onKeyDown      ==> cb.onEdit,
        ^.onChange       ==> cb.onChange
      )

    def link(label: StrValue, action: Option[ReactEvent ⇒ Callback]): ReactElement =
      Button(label.value, action, Button.Secondary, small = true, enabled = action.isDefined)
  }

  private val component = ReactComponentB[Props]("TableCell")
    .initialState_P(P ⇒ State(P.valueOpt))
    .renderBackend[Backend]
    .componentWillReceiveProps(receiveProps ⇒
      receiveProps.$.modState(_.copy(contentOpt = receiveProps.nextProps.valueOpt))
//      .when(receiveProps.currentProps.valueOpt =/= receiveProps.nextProps.valueOpt)
//      .void
    )
    .configure(ShouldUpdate.apply)
    .build

  def createMode(sendAction:         ReusableFn[Action, Callback],
                 cachedDataOpt:      Option[CachedData],
                 editorId:           EditorId,
                 col:                ColumnDesc,
                 valueOpt:           Option[StrValue],
                 validationErrorOpt: Option[ValidationError],
                 inputEnabled:       Boolean,
                 onUpdateOpt:        Option[StrValue ⇒ Callback]): ReactElement =

    component.withKey(col.ref.name.value)(
      Props(sendAction, createMode = true, inputEnabled, editorId, None, col, valueOpt, cachedDataOpt, validationErrorOpt, onUpdateOpt)
    )

  def apply(sendAction:         ReusableFn[Action, Callback],
            editorDesc:         EditorDesc,
            col:                ColumnDesc,
            rowIdOpt:           Option[StrRowId],
            cachedDataOpt:      Option[CachedData],
            valueOpt:           Option[StrValue],
            validationErrorOpt: Option[ValidationError],
            onUpdateOpt:        Option[StrValue ⇒ Callback]): ReactElement = {

    val inputEnabled: Boolean = {
      def tableMatches = col.ref.table =:= editorDesc.mainTable
      def isAllowed    = editorDesc.isEditable  && col.isEditable
      tableMatches && isAllowed && rowIdOpt.isDefined
    }

    component.withKey(col.ref.name.value + rowIdOpt.fold("")(_.value))(
      Props(sendAction, createMode = false, inputEnabled, editorDesc.editorId, rowIdOpt, col, valueOpt, cachedDataOpt, validationErrorOpt, onUpdateOpt)
    )
  }
}
