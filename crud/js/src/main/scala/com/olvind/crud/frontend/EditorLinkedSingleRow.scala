package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Reusability, ReusableFn}
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object EditorLinkedSingleRow {

  final case class Props(
    sendAction:    ReusableFn[Action, Callback],
    editor:        EditorDesc,
    cachedDataOpt: Option[CachedData],
    linkedRow:     StrLinkedRows,
    reload:        Callback,
    createElem:    ReactElement) {

    def row: StrTableRow =
      linkedRow.rows.head
  }

  final case class State(showCreate: Boolean)

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.by((P: Props) => (P.editor, P.cachedDataOpt, P.linkedRow))

  private implicit val ReusableState: Reusability[State] =
    Reusability.byRef[State]

  private final case class Backend($: BackendScope[Props, State]) {

    val toggleShowCreate: Callback =
      $.modState(S ⇒ S.copy(showCreate = !S.showCreate))

    def render(P: Props, S: State): ReactElement = {
      <.div(
        TableStyle.container,
        EditorToolbar(EditorToolbar.Props(
          editorDesc    = P.editor,
          rows          = 1,
          refresh       = P.reload,
          showAll       = P.sendAction(Navigate(RouteEditor(P.editor.editorId))),
          cachedDataOpt = P.cachedDataOpt,
          isLinkedOpt   = Some(P.linkedRow),
          deleteOpt     = P.row.idOpt.map(rowId ⇒ P.sendAction(DeleteRow(P.editor.editorId, rowId))),
          showCreateOpt = Some(toggleShowCreate)
        )),
        <.div(
          TableStyle.table,
          <.div(
            TableStyle.nested,
            P.createElem.some.filter(_ ⇒ S.showCreate)
          ),
          P.row.values.zip(P.editor.columns).map{
            case (value: StrValue, col) ⇒
              <.div(
                TableStyle.row,
                TableHeaderCell(TableHeaderCell.Props(
                  col,
                  P.editor.mainTable,
                  None
                )),

                TableCell(
                  sendAction         = P.sendAction,
                  editorDesc         = P.editor,
                  col                = col,
                  rowIdOpt           = P.row.idOpt,
                  cachedDataOpt      = P.cachedDataOpt,
                  valueOpt           = Some(value),
                  validationErrorOpt = None, //???,
//                  clearError         = clearValidationFail(P.row.idOpt),
                  onUpdateOpt        = P.row.idOpt.map(
                    rowId ⇒ (value: StrValue) ⇒ P.sendAction(UpdateValue(P.editor.editorId, rowId, col.ref, value))
                  )
              )
            )
          }
        )
      )
    }
  }
  
  private val component =
    ReactComponentB[Props]("EditorLinkedSingleRow")
      .initialState_P(P ⇒ State(showCreate = false))
      .renderBackend[Backend]
      .configure(ShouldUpdate.apply)
      .build

  def apply(p: Props): ReactElement =
    component(p)
}
