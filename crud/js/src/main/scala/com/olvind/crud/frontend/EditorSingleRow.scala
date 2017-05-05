package com.olvind.crud
package frontend

import autowire._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js
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
    validationFails: Map[Option[StrRowId], Seq[ValidationError]],
    data:            DataState,
    cachedDataOpt:   Option[CachedData],
    linkedRowsU:     U[Seq[StrLinkedRows]]
  ) extends StateBP[State]{

    override def withDataState(data: DataState): State =
      copy(data = data)

    override def withCachedData(cd: CachedData): State =
      copy(cachedDataOpt = Some(cd))

    override def withValidationFails(rowOpt: Option[StrRowId], ves: Seq[ValidationError]): State =
      copy(validationFails = validationFails.updated(rowOpt, ves))
  }

  private final case class Backend($: BackendScope[Props, State])
    extends BackendBUP[Props, State]{

    implicit override val r: Reusability[Props] =
      ComponentUpdates.InferredReusability[Props]

    override def handleDeleted(id: StrRowId): Callback =
      fromProps.value().showAllRows

    override def patchRow(id: StrRowId, row: StrTableRow): Callback =
      $.state.map(_.data).flatMap {
        case HasDataState(StrTableRow(Some(`id`), _)) ⇒ setData(HasDataState(row), Callback.empty)
        case _                                        ⇒ Callback.empty
      }

    override val loadInitialData: Callback =
      $.props.map(_.rowId).flatMap{ rowId =>
        async((user, remote) => remote.readRow(user, rowId).call(), None){
          case XSuccess(Some(row)) => setData(HasDataState(row), Callback.empty)
          case XSuccess(None)      => handleNoRowFoundOnUpdate(rowId)
        }
      }

    override def handleNoRowFoundOnUpdate(id: StrRowId): Callback =
      $.props.flatMap(P =>
        setData(
          ErrorState(XUserMsg(s"No row found with id ${P.rowId}")),
          Callback.empty
        )
      )

    val refreshLinkedRows: Callback =
      $.props.map(_.rowId).flatMap(rowId =>
        async((user, remote) => remote.readLinkedRows(user, rowId).call(), None){
          case XSuccess(linkedRows) => $.modState(_.copy(linkedRowsU = linkedRows))
        }
      )

    override def setData(data: DataState, cb: Callback): Callback =
      super.setData(data, cb) >> refreshLinkedRows

    override def renderData(P: Props, S: State, t: EditorDesc, row: StrTableRow): ReactElement = {
      <.div(TableStyle.container)(
        EditorToolbar(EditorToolbar.Props(
          editorDesc        = P.editorDesc,
          rows              = 1,
          cachedDataOpt     = S.cachedDataOpt,
          filterU           = js.undefined,
          openFilterDialogU = js.undefined,
          isLinkedU         = js.undefined,
          refreshU          = reInit,
          showAllU          = fromProps.value().showAllRows,
          deleteU           = deleteRow(P.rowId),
          showCreateU       = (false, P.base.ctl.setEH(RouteCreateRow(P.editorDesc))),
          customElemU       = js.undefined
        )),

        renderRow(S, t)(row),

        <.div(
          TableStyle.nested,
          S.linkedRowsU map {
            case linkedRows ⇒
              linkedRows map { linkedRow ⇒
                val colIdx:  Int         = P.editorDesc.columns indexWhere (_.ref =:= linkedRow.fromCol)
                val colIdxU: U[Int]      = colIdx.uSome filter (_ =/= -1)
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
              TableHeaderCell(TableHeaderCell.Props(
                col,
                t.mainTable,
                js.undefined
              )),
              TableCell(
                clearValidationFail(row.idOpt),
                S.cachedDataOpt,
                row.idOpt.map(updateValue).asUndef,
                fromProps.value().showSingleRow)(
                t, col, uid, uv, ue)
            )
        )
      )
  }

  private val component =
    ReactComponentB[Props]("EditorSingleRow")
      .initialState_P(P ⇒
        State(
          Map.empty,
          InitialState,
          P.base.cachedDataOpt,
          js.undefined
        )
      )
      .backend(Backend)
      .render($ ⇒ $.backend.render($.props, $.state))
      .configure(ComponentUpdates.inferred("EditorSingleRow"))
      .componentDidMount(_.backend.init)
      .build

  def apply(p: Props): ReactElement =
    component.withKey(p.base.editorDesc.editorId.value + p.rowId.value)(p)
}
