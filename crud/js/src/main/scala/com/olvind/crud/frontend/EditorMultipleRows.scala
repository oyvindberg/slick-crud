package com.olvind.crud
package frontend

import autowire._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom

import scala.scalajs.js
import scalacss.ScalaCssReact._

object EditorMultipleRows
  extends EditorBaseMultipleRows
  with EditorBaseUpdaterPrimary {

  case class Props (
    base: EditorBaseProps
  ) extends PropsB

  case class State(
    validationFails:  Map[Option[StrRowId], Seq[ValidationError]],
    params:           QueryParams,
    data:             DataState,
    cachedDataOpt:    Option[CachedData],
    isUpdating:       Boolean,
    isAtBottom:       Boolean,
    filterDialogOpen: Boolean) extends StateBP[State]{

    override def withDataState(data: DataState): State =
      copy(data = data)

    override def withCachedData(cd: CachedData): State =
      copy(cachedDataOpt = cd.some)

    override def withValidationFails(rowOpt: Option[StrRowId], ves: Seq[ValidationError]): State =
      copy(validationFails = validationFails.updated(rowOpt, ves))
  }

  private final case class Backend($: BackendScope[Props, State])
    extends BackendBUP[Props, State]
    with OnUnmount {

    override implicit val r = ComponentUpdates.InferredReusability[Props]

    override def handleDeleted(id: StrRowId): Callback =
      $.state.map(_.data).flatMap{
        case HasDataState(rows) ⇒
          setData(HasDataState(rows.filterNot(_.idOpt =:= id.some)), Callback.empty)
        case _ ⇒ Callback.empty
      }

    override def patchRow(id: StrRowId, row: StrTableRow): Callback =
      $.state.map(_.data) flatMap {
        case HasDataState(rows) ⇒
          val StableId = id.some
          val newRows = rows map {
            case StrTableRow(StableId, _) ⇒ row
            case otherRow                 ⇒ otherRow
          }
          setData(HasDataState(newRows), Callback.empty)
        case _ ⇒ Callback.empty
      }

    override def handleNoRowFoundOnUpdate(id: StrRowId): Callback =
      $.state.map(_.data) flatMap {
        case HasDataState(rows) ⇒
          val newRows = rows.filterNot(_.idOpt =:= Some(id))
          setData(HasDataState(newRows), Callback.empty)
        case _ ⇒ Callback.empty
      }

    override val loadInitialData: Callback =
      $.state.flatMap(S ⇒ fetchRows(S.data, S.params.copy(page = PageNum.zero), append = false))

    def fetchRows(dataState: DataState, params: QueryParams, append: Boolean): Callback =
      $.modState(_.copy(isUpdating = true)) >>
      async((user, remote) => remote.read(user, params.some).call(), None){
        case XSuccess(rows) =>
          val newData = (dataState, append) match {
            case (HasDataState(existing), true) ⇒ HasDataState(existing ++ rows)
            case _                              ⇒ HasDataState(rows)
          }
          setData(newData, $.modState(_.copy(params = params, isUpdating = false)))
      }

    val openFilteringDialog: Callback =
      $.modState(_.copy(filterDialogOpen = true))

    val closeFilteringDialog: Callback =
      $.modState(_.copy(filterDialogOpen = false))

    def onFilteringChanged(S: State): Option[Filter] ⇒ Callback =
      of ⇒ fetchRows(S.data, S.params.withFilter(of), append = false)

    def onSort(S: State): ColumnRef ⇒ Callback =
      c ⇒ fetchRows(S.data, S.params.withSortedBy(c), append = false)

    val onScroll: dom.UIEvent ⇒ Callback = {
      e ⇒
        val w          = e.currentTarget.asInstanceOf[dom.Window]
        val d          = e.target.asInstanceOf[dom.Document]
        val scrollY    = w.asInstanceOf[js.Dynamic].scrollY.asInstanceOf[Double]
        val y          = d.documentElement.scrollHeight - w.innerHeight
        val isAtBottom = scrollY / y > 0.98

        $.state.flatMap{
          S ⇒
            $.modState(
              _.copy(isAtBottom = isAtBottom),
              fetchRows(S.data, S.params.withNextPage, append = true)
            ).when(isAtBottom != S.isAtBottom && !S.isUpdating).void
        }
    }

    override def renderData(P: Props, S: State, editorDesc: EditorDesc, rows: Seq[StrTableRow]): ReactElement = {
      val fp = fromProps.value()

      val filterDialog = FilteringDialog(FilteringDialog.Props(
        cols           = P.base.editorDesc.columns,
        initial        = S.params.filter,
        onParamsChange = onFilteringChanged(S),
        cachedDataOpt  = S.cachedDataOpt,
        dialogOpen     = S.filterDialogOpen,
        closeDialog    = closeFilteringDialog
      ))

      <.div(
        TableStyle.container,
        filterDialog,
        EditorToolbar(EditorToolbar.Props(
          editorDesc        = P.editorDesc,
          rows              = rows.size,
          cachedDataOpt     = S.cachedDataOpt,
          filterU           = S.params.filter.asUndef,
          openFilterDialogU = openFilteringDialog,
          isLinkedU         = js.undefined,
          refreshU          = reInit,
          showAllU          = js.undefined,
          deleteU           = js.undefined,
          showCreateU       = (false, P.base.ctl.setEH(RouteCreateRow(P.editorDesc))),
          customElemU       = js.undefined
        )),
        <.div(
          TableStyle.table,
          TableHeader(TableHeader.Props(
            editorDesc = editorDesc,
            sortingU   = S.params.sorting.asUndef,
            onSort     = onSort(S)
          )),
          rows.map(row ⇒
            TableRow(TableRow.Props(
                editorDesc      = editorDesc,
                row             = row,
                cachedDataOpt   = S.cachedDataOpt,
                onUpdateU       = row.idOpt.asUndef.map(updateValue),
                showSingleRow   = fp.showSingleRow,
                validationFails = S.validationFails,
                clearError      = clearValidationFail(row.idOpt)
              ))
          ),
          S.isUpdating ?= renderWaiting
        )
      )
    }
  }

  private val component =
    ReactComponentB[Props]("EditorMultipleRows")
      .initialState_P(P ⇒
        State(
          Map.empty,
          QueryParams(QueryParams.defaultPageSize, PageNum.zero, sorting = None, filter = None),
          InitialState,
          P.base.cachedDataOpt,
          isUpdating = false,
          isAtBottom = false,
          filterDialogOpen = false
        )
      )
      .backend(Backend)
      .render($ ⇒ $.backend.render($.props, $.state))
      .configure(ComponentUpdates.inferred("EditorMultipleRows"))
      .configure(EventListener[dom.UIEvent].install("scroll", _.backend.onScroll, _ => dom.window))
      .componentDidMount(_.backend.init)
      .build

  def apply(p: Props): ReactElement =
    component.withKey(p.editorDesc.editorId.value)(p)
}

