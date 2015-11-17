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
    validationFails:  Seq[ValidationError],
    params:           QueryParams,
    data:             DataState,
    cachedDataU:      U[CachedData],
    isUpdating:       Boolean,
    isAtBottom:       Boolean,
    filterDialogOpen: Boolean) extends StateBP[State]{

    override def withDataState(data: DataState) =
      copy(data = data)

    override def withCachedData(cd: CachedData) =
      copy(cachedDataU = cd)

    override def withValidationFails(ves: Seq[ValidationError]) =
      copy(validationFails = ves)
  }

  final case class Backend($: BackendScope[Props, State])
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
      fromProps.value().asyncCb.applyEither(
        "Couldn't read rows",
        remote => u => remote.read(u, params.some).call())()
        .commit {
          read ⇒
            val newData = (dataState, append) match {
              case (HasDataState(existing), true) ⇒ HasDataState(existing ++ read.rows)
              case _                              ⇒ HasDataState(read.rows)
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
            ).filter(isAtBottom != S.isAtBottom && !S.isUpdating)
        }
    }

    override def renderData(P: Props, S: State, table: EditorDesc, rows: Seq[StrTableRow]): ReactElement = {
      val f = FilteringDialog()(FilteringDialog.Props(
        cols           = P.base.editorDesc.columns,
        initial        = S.params.filter,
        onParamsChange = onFilteringChanged(S),
        cachedDataU    = S.cachedDataU,
        dialogOpen     = S.filterDialogOpen,
        closeDialog    = closeFilteringDialog
      ))

      <.div(
        TableStyle.container,
        f,
        EditorToolbar()(EditorToolbar.Props(
          editorDesc        = P.editorDesc,
          rows              = rows.size,
          cachedDataU       = S.cachedDataU,
          filterU           = S.params.filter.asUndef,
          openFilterDialogU = openFilteringDialog,
          isLinkedU         = uNone,
          refreshU          = reInit,
          showAllU          = uNone,
          deleteU           = uNone,
          showCreateU       = (false, P.base.ctl.setEH(RouteCreateRow(P.editorDesc))),
          customElemU       = uNone
        )),
        <.div(
          TableStyle.table,
          TableHeader()(TableHeader.Props(
            table,
            S.params.sorting.asUndef,
            onSort(S)
          )),
          rows.map(
            r ⇒ TableRow(r)(TableRow.Props(
              table,
              r,
              S.cachedDataU,
              r.idOpt.asUndef.map(updateValue),
              fromProps.value().showSingleRow,
              S.validationFails,
              clearValidationFail(r.idOpt)
            ))
          ),
          S.isUpdating ?= renderWaiting
        )
      )
    }
  }

  val component = ReactComponentB[Props]("EditorMultipleRows")
    .initialState_P(P ⇒
      State(
        Seq.empty,
        QueryParams(QueryParams.defaultPageSize, PageNum.zero, sorting = None, filter = None),
        InitialState,
        P.base.cachedDataF.currentValueU,
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

  def apply(editorDesc: EditorDesc) =
    component.withKey(editorDesc.editorId.value)
}

