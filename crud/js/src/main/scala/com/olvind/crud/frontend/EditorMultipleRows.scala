package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom

import scala.scalajs.js
import scalacss.ScalaCssReact._

object EditorMultipleRows {

  final case class Props (
    sendAction:    ReusableFn[Action, Callback],
    model:         EditorModel,
    cachedDataOpt: Option[CachedData]
  )

  final case class State(
    isUpdating:       Boolean,
    isAtBottom:       Boolean,
    filterDialogOpen: Boolean
  )

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.caseClass[Props]
  private implicit val ReusableState: Reusability[State] =
    Reusability.caseClass[State]

  private final case class Backend($: BackendScope[Props, State])
    extends OnUnmount {

    val loadData: Callback =
      $.props.flatMap(
        P => P.sendAction(FetchEditorData(P.model.editor.editorId))
      )

    val openFilteringDialog: Callback =
      $.modState(_.copy(filterDialogOpen = true))

    val closeFilteringDialog: Callback =
      $.modState(_.copy(filterDialogOpen = false))

    val onSort: ReusableFn[ColumnRef, Callback] =
      ReusableFn(c ⇒ $.props.flatMap(P ⇒ P.sendAction(FetchSortedData(P.model.editor.editorId, c))))

    val onScroll: dom.UIEvent ⇒ Callback = {
      e ⇒
        val w          = e.currentTarget.asInstanceOf[dom.Window]
        val d          = e.target.asInstanceOf[dom.Document]
        val scrollY    = w.asInstanceOf[js.Dynamic].scrollY.asInstanceOf[Double]
        val y          = d.documentElement.scrollHeight - w.innerHeight
        val isAtBottom = scrollY / y > 0.98

        $.props.zip($.state).flatMap{
          case (p, s) ⇒
            $.modState(
              _.copy(isAtBottom = isAtBottom),
              p.sendAction(FetchMoreData(p.model.editor.editorId))
            ).when(isAtBottom != s.isAtBottom && !s.isUpdating).void
        }
    }

    def render(P: Props, S: State): ReactElement = {
      val filterDialog = FilteringDialog(FilteringDialog.Props(
        sendAction     = P.sendAction,
        editorId       = P.model.editor.editorId,
        cols           = P.model.editor.columns,
        initial        = P.model.queryParams.filterOpt,
        cachedDataOpt  = P.cachedDataOpt,
        dialogOpen     = S.filterDialogOpen,
        closeDialog    = closeFilteringDialog
      ))

      def toolbar(numRows: Int) =
        EditorToolbar(EditorToolbar.Props(
          editorDesc          = P.model.editor,
          rows                = numRows,
          showAll             = P.sendAction(Navigate(RouteEditor(P.model.editor.editorId))),
          cachedDataOpt       = P.cachedDataOpt,
          filterOpt           = P.model.queryParams.filterOpt,
          openFilterDialogOpt = Some(openFilteringDialog),
          refresh             = loadData,
          showCreateOpt       = Some(P.sendAction(Navigate(RouteCreateRow(P.model.editor.editorId))))
        ))

      WaitingRows(P.model.rows){
        rows ⇒
          <.div(
            TableStyle.container,
            filterDialog,
            toolbar(rows.size),
            <.div(
              TableStyle.table,
              TableHeader(TableHeader.Props(
                editor     = P.model.editor,
                sortingOpt = P.model.queryParams.sortingOpt,
                onSortOpt  = Some(onSort)
              )),
              rows.map(row ⇒
                TableRow(TableRow.Props(
                  sendAction      = P.sendAction,
                  editor          = P.model.editor,
                  row             = row,
                  cachedDataOpt   = P.cachedDataOpt,
                  validationFails = P.model.validationErrors.get(row.idOpt)
                ))
              ),
              S.isUpdating ?= WaitingRows.renderWaiting
            )
          )
      }
    }
  }

  private val component =
    ReactComponentB[Props]("EditorMultipleRows")
      .initialState(State(isUpdating = false, isAtBottom = false, filterDialogOpen = false))
      .renderBackend[Backend]
      .configure(ShouldUpdate.apply)
      .configure(EventListener[dom.UIEvent].install("scroll", _.backend.onScroll, _ => dom.window))
      .componentWillMount(_.backend.loadData)
      .build

  def apply(p: Props): ReactElement =
    component.withKey(p.model.editor.editorId.value)(p)
}

