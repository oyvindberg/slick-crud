package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Reusability, ReusableFn}
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object EditorCreateRow {

  final case class Props(
    sendAction:    ReusableFn[Action, Callback],
    model:         EditorModel,
    cachedDataOpt: Option[CachedData],
    wasLinkedOpt:  Option[StrLinkedRows],
    prefilled:     Map[ColumnRef, StrValue])  {

    def cols: List[ColumnDesc] =
      model.editor.mainCols.filterNot(_.ref.isAutoInc)

    def initialValues: Map[ColumnRef, Option[StrValue]] =
      cols.map (
         col => col.ref -> prefilled.get(col.ref)
      ).toMap
  }

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.caseClass[Props]
  private implicit val ReusableState: Reusability[State] =
    Reusability.byRef[State]

  final case class State(values: Map[ColumnRef, Option[StrValue]])

  private final case class Backend($: BackendScope[Props, State]){

    val reInit: Callback =
      $.props.flatMap(
        P => $.modState(_.copy(values = P.initialValues))
      )

    def setColValue(c: ColumnRef)(str: StrValue): Callback =
      $.modState(s ⇒ s.copy(values = s.values.updated(c, Some(str))))

    def trySave(P: Props, values: Map[ColumnRef, Option[StrValue]]): Callback =
      P.sendAction(
        CreateRow(
          P.model.editor.editorId,
          values mapValues (_ getOrElse StrValue(""))
        )
      )

    def toolbar(P: Props, S: State): ReactElement =
      EditorToolbar(EditorToolbar.Props(
        editorDesc    = P.model.editor,
        rows          = 0,
        refresh       = reInit,
        showAll       = P.sendAction(Navigate(RouteEditor(P.model.editor.editorId))),
        cachedDataOpt = P.cachedDataOpt,
        isLinkedOpt   = P.wasLinkedOpt,
        customElemOpt = Some(Button("Save", Some((e: ReactEvent) ⇒ trySave(P, S.values)), Button.Primary))
      ))

    def render(P: Props, S: State): ReactElement = {
      <.div(
        TableStyle.centered,
        <.div(TableStyle.container)(
          toolbar(P, S),
          <.div(
            TableStyle.table,
            P.cols.map{
              col ⇒
                val validationErrorOpt: Option[ValidationError] =
                  P.model.validationErrors.get(None).flatMap(_.get(col.ref))

                <.div(
                  TableStyle.row,
                  TableHeaderCell(TableHeaderCell.Props(
                    col       = col,
                    mainTable = P.model.editor.mainTable,
                    sortedOpt = None
                  )),
                  TableCell.createMode(
                    sendAction         = P.sendAction,
                    cachedDataOpt      = P.cachedDataOpt,
                    editorId           = P.model.editor.editorId,
                    col                = col,
                    valueOpt           = S.values.get(col.ref).flatten,
                    validationErrorOpt = validationErrorOpt,
                    inputEnabled       = P.wasLinkedOpt.map(_.toCol) =/= Some(col.ref),
                    onUpdateOpt        = Some(setColValue(col.ref) _)
                  )
                )
            }
          )
        )
      )
    }
  }

  private val component =
    ReactComponentB[Props]("EditorCreateRow")
    .initialState(State(Map.empty[ColumnRef, Option[StrValue]]))
    .renderBackend[Backend]
    .componentDidMount($ => $.backend.reInit)
    .configure(ShouldUpdate.apply)
    .build

  def apply(p: Props): ReactElement =
    component.withKey(p.model.editor.editorId.value)(p)
}
