package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js
import scalacss.ScalaCssReact._

object EditorLinkedSingleRow
  extends EditorBaseSingleRow
  with EditorBaseUpdaterLinked {

  final case class Props(
    base:       EditorBaseProps,
    linkedRow:  StrLinkedRows,
    reload:     Callback,
    createElem: ReactElement) extends PropsBUL {

    def row: StrTableRow =
      linkedRow.rows.head
  }

  final case class State(
    validationFails: Map[Option[StrRowId], Seq[ValidationError]],
    showCreate:      Boolean,
    cachedDataOpt:   Option[CachedData]) extends StateB[State]{

    override def withCachedData(cd: CachedData): State =
      copy(cachedDataOpt = cd.some)

    override def withValidationFails(rowOpt: Option[StrRowId], ves: Seq[ValidationError]): State =
      copy(validationFails = validationFails.updated(rowOpt, ves))
  }

  private final case class Backend($: BackendScope[Props, State])
    extends BackendBUL[Props, State]{

    override implicit val r = ComponentUpdates.InferredReusability[Props]

    val toggleShowCreate: ReactEvent ⇒ Callback =
      e ⇒ $.modState(S ⇒ S.copy(showCreate = !S.showCreate))

    override def render(P: Props, S: State): ReactElement = {
      val fp = fromProps.value()
      <.div(
        TableStyle.container,
        EditorToolbar(EditorToolbar.Props(
          editorDesc        = P.editorDesc,
          rows              = 1,
          cachedDataOpt     = S.cachedDataOpt,
          filterU           = js.undefined,
          openFilterDialogU = js.undefined,
          isLinkedU         = P.linkedRow,
          refreshU          = P.reload,
          showAllU          = fp.showAllRows,
          deleteU           = P.row.idOpt.asUndef map deleteRow,
          showCreateU       = (S.showCreate, toggleShowCreate),
          customElemU       = js.undefined
        )),
        <.div(
          TableStyle.table,
          <.div(
            TableStyle.nested,
            P.createElem.some.filter(_ ⇒ S.showCreate)
          ),
          forColumns(P.editorDesc, P.row, S.validationFails)(
            (t, col, uid, uv, ue) ⇒
              <.div(
                TableStyle.row,
                TableHeaderCell(TableHeaderCell.Props(
                  col,
                  t.mainTable,
                  js.undefined
                )),
                TableCell(
                  clearError     = clearValidationFail(P.row.idOpt),
                  cachedDataOpt  = S.cachedDataOpt,
                  updateU        = P.row.idOpt.map(updateValue).asUndef,
                  showSingleRowU = fp.showSingleRow)(
                  t, col, uid, uv, ue)
              )
          )
        )
      )
    }
  }
  
  private val component =
    ReactComponentB[Props]("EditorSingleRow")
    .initialState_P(P ⇒ State(Map.empty, showCreate = false, P.base.cachedDataOpt))
    .renderBackend[Backend]
    .configure(ComponentUpdates.inferred("EditorSingleRow"))
    .componentDidMount(_.backend.init)
    .build

  def apply(p: Props): ReactElement =
    component(p)
}
