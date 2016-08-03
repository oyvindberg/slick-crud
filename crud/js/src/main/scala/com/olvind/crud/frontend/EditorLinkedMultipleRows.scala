package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object EditorLinkedMultipleRows  {

  final case class Props (
    sendAction:    ReusableFn[Action, Callback],
    editor:        EditorDesc,
    cachedDataOpt: Option[CachedData],
    linkedRows:    StrLinkedRows,
    reload:        Callback,
    createElem:    ReactElement
  )

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.by((P: Props) => (P.editor, P.cachedDataOpt, P.linkedRows))
  private implicit val ReusableState: Reusability[State] =
    Reusability.caseClass[State]

  final case class State(showCreate: Boolean)

  private final case class Backend($: BackendScope[Props, State]) extends OnUnmount {

    val toggleShowCreate: Callback =
      $.modState(S ⇒ S.copy(showCreate = !S.showCreate))

    def render(P: Props, S: State): ReactElement = {
      <.div(
        TableStyle.container,
        EditorToolbar(EditorToolbar.Props(
          editorDesc          = P.editor,
          rows                = P.linkedRows.rows.size,
          cachedDataOpt       = P.cachedDataOpt,
          refresh             = P.reload,
          showAll             = P.sendAction(Navigate(RouteEditor(P.editor.editorId))),
          isLinkedOpt         = Some(P.linkedRows),
          showCreateOpt       = Some(toggleShowCreate)
        )),
        <.div(
          TableStyle.table,
          <.div(
            TableStyle.nested,
            P.createElem.some.filter(_ ⇒ S.showCreate)
          ),
          TableHeader(TableHeader.Props(
            editor     = P.editor,
            sortingOpt = None,
            onSortOpt  = None
          )),
          P.linkedRows.rows.map(
            (row: StrTableRow) ⇒
              TableRow(TableRow.Props(
                sendAction      = P.sendAction,
                editor          = P.editor,
                row             = row,
                cachedDataOpt   = P.cachedDataOpt,
                validationFails = None//??? //P.validationFails
              ))
          )
        )
      )
    }
  }

  private val component =
    ReactComponentB[Props]("EditorMultipleRows")
      .initialState(State(showCreate = false))
      .renderBackend[Backend]
      .configure(ShouldUpdate.apply)
      .componentDidMount($ ⇒ $.props.sendAction(FetchEditorData($.props.editor.editorId)))
      .build

  def apply(p: Props): ReactElement =
    component.withKey(s"${p.linkedRows.fromCol}_${p.linkedRows.toCol}" + p.linkedRows.rows.size)(p)
}

