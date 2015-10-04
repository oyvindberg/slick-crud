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
    validationFails: Seq[ValidationError],
    data:            DataState,
    cachedDataU:     U[CachedData],
    linkedRowsU:     U[Seq[StrLinkedRows]]
  ) extends StateBP[State]{

    override def withDataState(data: DataState) =
      copy(data = data)

    override def withCachedData(cd: CachedData) =
      copy(cachedDataU = cd)

    override def withValidationFails(ves: Seq[ValidationError]) =
      copy(validationFails = ves)
  }

  final case class Backend($: BackendScope[Props, State])
    extends BackendBUP[Props, State]{

    implicit override val r = ComponentUpdates.InferredReusability[Props]

    override def handleDeleted(id: StrRowId): Callback =
      fromProps.value().showAllRows

    override def patchRow(id: StrRowId, row: StrTableRow): Callback = {
      val StableId = id.some
      $.state.map(_.data).flatMap {
        case HasDataState(StrTableRow(StableId, _)) ⇒ setData(HasDataState(row), Callback.empty)
        case _                                      ⇒ Callback.empty
      }
    }

    override val loadInitialData: Callback =
      $.props.map(_.rowId).flatMap{ rowId =>
        fromProps.value().asyncCb.applyEither(
          s"Couldn't load row $rowId", r => u => r.readRow(u, rowId).call())()
          .commit(read ⇒ read.row match {
            case Some(row) => setData(HasDataState(row), Callback.empty)
            case None      => handleNoRowFoundOnUpdate(rowId)
          })
      }

    override def handleNoRowFoundOnUpdate(id: StrRowId): Callback =
      $.props.flatMap(P =>
        setData(
          ErrorState(CrudException(
            P.editorDesc.editorId,
            ErrorMsg(s"No row found with id ${P.rowId}"),
            "")),
          Callback.empty
        )
      )

    def refreshLinkedRows: Callback =
      $.props.map(_.rowId).flatMap(rowId =>
        fromProps.value().asyncCb.applyEither(
          s"Couldn't read linked rows for $rowId",
          r => u => r.readLinkedRows(u, rowId).call())(
          ignoreError
        ).commit(read ⇒ $.modState(_.copy(linkedRowsU = read.linkedRows)))
      )

    override def setData(data: DataState, cb: Callback): Callback =
      super.setData(data, cb) >> refreshLinkedRows

    override def renderData(P: Props, S: State, t: EditorDesc, row: StrTableRow): ReactElement = {
      <.div(TableStyle.container)(
        EditorToolbar()(EditorToolbar.Props(
          editorDesc        = P.editorDesc,
          rows              = 1,
          cachedDataU       = S.cachedDataU,
          filterU           = uNone,
          openFilterDialogU = uNone,
          isLinkedU         = uNone,
          refreshU          = reInit,
          showAllU          = fromProps.value().showAllRows,
          deleteU           = deleteRow(P.rowId),
          showCreateU       = (false, P.base.ctl.setEH(RouteCreateRow(P.editorDesc))),
          customElemU       = uNone
        )),

        renderRow(S, t)(row),
        <.div(
          TableStyle.nested,
          S.linkedRowsU map {
            case linkedRows ⇒
              linkedRows map { linkedRow ⇒
                val colIdx:  Int         = P.editorDesc.columns indexWhere (_.ref =:= linkedRow.fromCol)
                val colIdxU: U[Int]      = colIdx.undef filter (_ =/= -1)
                val viaValU: U[StrValue] = colIdxU map row.values

                P.renderLinked(loadInitialData, linkedRow, viaValU)
              }
          }
        )
      )
    }

    def renderRow(S: State, t: EditorDesc)(row: StrTableRow): ReactElement =
      <.div(
        TableStyle.table,
        forColumns(t, row, S.validationFails)(
          (t, col, uid, uv, ue) ⇒
            <.div(
              TableStyle.row,
              TableHeaderCell(col)(TableHeaderCell.Props(
                col,
                t.mainTable,
                uNone
              )),
              TableCell(
                clearValidationFail(row.idOpt),
                S.cachedDataU,
                row.idOpt.map(updateValue).asUndef,
                fromProps.value().showSingleRow)(
                t, col, uid, uv, ue)
            )
        )
      )
  }
  
  val component = ReactComponentB[Props]("EditorSingleRow")
    .initialState_P(P ⇒
      State(
        Seq.empty,
        InitialState,
        P.base.cachedDataF.currentValueU,
        uNone
      )
    )
    .backend(Backend)
    .render($ ⇒ $.backend.render($.props, $.state))
    .configure(ComponentUpdates.inferred("EditorSingleRow"))
    .componentDidMount(_.backend.init)
    .build

  def apply(editorDesc: EditorDesc, rowId: StrRowId) =
    component.withKey(editorDesc.editorId.value + rowId.value)
}
