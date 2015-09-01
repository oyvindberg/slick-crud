package com.olvind.crud
package frontend

import autowire._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object EditorSingleRow
  extends EditorBaseSingleRow
  with EditorBaseUpdaterPrimary{

  final case class Props(
    base:         EditorBaseProps,
    rowId:        StrRowId,
    renderLinked: (Callback, StrLinkedRows, U[StrValue]) ⇒ ReactNode
  ) extends PropsB

  final case class State(
    data:        DataState,
    cachedDataU: U[CachedData],
    linkedRowsU: U[Seq[StrLinkedRows]]
  ) extends StateBP[State]{

    override def withDataState(data: DataState) =
      copy(data = data)

    override def withCachedData(cd: CachedData) =
      copy(cachedDataU = cd)
  }

  final case class Backend($: WrapBackendScope[Props, State])
    extends BackendBUP[Props, State]{

    override def handleDeleted(id: StrRowId): Callback =
      showAllRows

    override def patchRow(id: StrRowId, row: StrTableRow): Callback = {
      val StableId = id.some
      $.state.map(_.data).flatMap {
        case HasDataState(StrTableRow(StableId, _)) ⇒ setData(HasDataState(row), Callback.empty)
        case _                                      ⇒ Callback.empty
      }
    }

    override val loadInitialData: Callback =
      asyncCb(s"Couldn't load row ${$.props.rowId}", remote.readRow($.props.base.userInfo, $.props.rowId).call())
        .commit(read ⇒ setData(HasDataState(read.row), Callback.empty))

    val refreshLinkedRows: Callback =
      asyncCb(s"Couldn't read linked rows for ${$.props.rowId}", remote.readLinkedRows($.props.base.userInfo, $.props.rowId).call())
        .commit(read ⇒ $.modState(_.copy(linkedRowsU = read.linkedRows)))

    override def setData(data: DataState, cb: Callback): Callback =
      super.setData(data, cb) >> refreshLinkedRows

    override def renderData(S: State, t: ClientTable, row: StrTableRow): ReactElement = {
      <.div(TableStyle.container)(
        EditorToolbar()(EditorToolbar.Props(
          table             = $.props.table,
          rows              = 1,
          cachedDataU       = S.cachedDataU,
          filterU           = uNone,
          openFilterDialogU = uNone,
          isLinkedU         = uNone,
          refreshU          = reInit,
          showAllU          = showAllRows,
          deleteU           = deleteRow($.props.rowId),
          showCreateU       = (false, $.props.base.ctl.setEH(RouteCreateRow($.props.table))),
          customElemU       = uNone
        )),

        renderRow(S, t)(row),
        <.div(
          TableStyle.nested,
          S.linkedRowsU map {
            case linkedRows ⇒
              linkedRows map { linkedRow ⇒
                val colIdx:  Int         = $.props.table.columns indexWhere (_.column =:= linkedRow.fromCol)
                val colIdxU: U[Int]      = colIdx.undef filter (_ =/= -1)
                val viaValU: U[StrValue] = colIdxU map row.values

                $.props.renderLinked(loadInitialData, linkedRow, viaValU)
              }
          }
        )
      )
    }

    def renderRow(S: State, t: ClientTable)(row: StrTableRow): ReactElement =
      <.div(
        TableStyle.table,
        forColumns(t, row)(
          (t, col, uid, uv) ⇒
            <.div(
              TableStyle.row,
              TableHeaderCell(col)(TableHeaderCell.Props(
                col,
                t.name,
                uNone
              )),
              TableCell(
                S.cachedDataU,
                row.idOpt.map(updateValue).asUndef,
                showSingleRow)(
                t, col, uid, uv)
            )
        )
      )
  }
  
  val component = ReactComponentB[Props]("EditorSingleRow")
    .initialState_P(P ⇒
      State(
        InitialState,
        P.base.cachedDataF.currentValueU,
        uNone
      )
    )
    .backend($ ⇒ Backend(WrapBackendScope($)))
    .render($ ⇒ $.backend.render($.props, $.state))
    .configure(ComponentUpdates.inferred("EditorSingleRow"))
    .componentDidMount(_.backend.init)
    .build

  def apply(table: ClientTable, rowId: StrRowId) =
    component.withKey(table.name.value + rowId.value)
}
