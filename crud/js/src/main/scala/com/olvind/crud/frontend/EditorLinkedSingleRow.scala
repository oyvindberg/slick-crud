package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object EditorLinkedSingleRow
  extends EditorBaseSingleRow
  with EditorBaseUpdaterLinked {

  final case class Props(
    base:       EditorBaseProps,
    linkedRow:  StrLinkedRows,
    reload:     Callback,
    createElem: ReactElement) extends PropsBUL {
    def row = linkedRow.rows.head
  }

  final case class State(
    showCreate:  Boolean,
    cachedDataU: U[CachedData]) extends StateB[State]{
    override def withCachedData(cd: CachedData) =
      copy(cachedDataU = cd)
  }

  final case class Backend($: WrapBackendScope[Props, State])
    extends BackendBUL[Props, State]{

    val toggleShowCreate: ReactEvent ⇒ Callback =
      e ⇒ $.modState(S ⇒ S.copy(showCreate = !S.showCreate))

    override def render(P: Props, S: State): ReactElement = {
      <.div(TableStyle.container)(
        EditorToolbar()(EditorToolbar.Props(
          table             = $.props.table,
          rows              = 1,
          cachedDataU       = S.cachedDataU,
          filterU           = uNone,
          openFilterDialogU = uNone,
          isLinkedU         = P.linkedRow,
          refreshU          = P.reload,
          showAllU          = showAllRows,
          deleteU           = P.row.idOpt.asUndef map deleteRow,
          showCreateU       = (S.showCreate, toggleShowCreate),
          customElemU       = uNone
        )),
        <.div(
          TableStyle.table,
          <.div(
            TableStyle.nested,
            P.createElem.some.filter(_ ⇒ S.showCreate)
          ),
          forColumns(P.table, P.row)(
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
                  P.row.idOpt.map(updateValue).asUndef,
                  showSingleRow)(
                  t, col, uid, uv)
              )
          )
        )
      )
    }
  }
  
  val component = ReactComponentB[Props]("EditorSingleRow")
    .initialState_P(P ⇒ State(showCreate = false, P.base.cachedDataF.currentValueU))
    .backend($ ⇒ Backend(WrapBackendScope($)))
    .render($ ⇒ $.backend.render($.props, $.state))
    .configure(ComponentUpdates.inferred("EditorSingleRow"))
    .componentDidMount(_.backend.init)
    .build

  def apply() =
    component
}
