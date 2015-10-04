package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object EditorLinkedMultipleRows
  extends EditorBaseMultipleRows
  with EditorBaseUpdaterLinked {

  case class Props (
    base:       EditorBaseProps,
    linkedRows: StrLinkedRows,
    reload:     Callback,
    createElem: ReactElement) extends PropsBUL {
    def rows = linkedRows.rows
  }

  case class State(
    validationFails: Seq[ValidationError],
    showCreate:      Boolean,
    cachedDataU:     U[CachedData]) extends StateB[State]{

    override def withCachedData(cd: CachedData) =
      copy(cachedDataU = cd)

    override def withValidationFails(ves: Seq[ValidationError]) =
      copy(validationFails = ves)
  }

  final case class Backend($: BackendScope[Props, State])
    extends BackendBUL[Props, State]
    with OnUnmount {

    override implicit val r = ComponentUpdates.InferredReusability[Props]

    val toggleShowCreate: ReactEvent ⇒ Callback =
      e ⇒ $.modState(S ⇒ S.copy(showCreate = !S.showCreate))

    override def render(P: Props, S: State): ReactElement = {
      <.div(
        TableStyle.container,
        EditorToolbar()(EditorToolbar.Props(
          editorDesc        = P.editorDesc,
          rows              = P.rows.size,
          cachedDataU       = S.cachedDataU,
          filterU           = uNone,
          openFilterDialogU = uNone,
          isLinkedU         = P.linkedRows,
          refreshU          = reInit,
          showAllU          = fromProps.value().showAllRows,
          deleteU           = uNone,
          showCreateU       = (S.showCreate, toggleShowCreate),
          customElemU       = uNone
        )),
        <.div(
          TableStyle.table,
          <.div(
            TableStyle.nested,
            P.createElem.some.filter(_ ⇒ S.showCreate)
          ),
          TableHeader()(TableHeader.Props(
            editorDesc    = P.editorDesc,
            sortingU = uNone,
            onSort   = uNone
          )),
          P.rows.map(
            r ⇒ TableRow(r)(TableRow.Props(
              P.editorDesc,
              r,
              S.cachedDataU,
              r.idOpt.asUndef.map(updateValue),
              fromProps.value().showSingleRow,
              S.validationFails,
              clearValidationFail(r.idOpt)
            ))
          )
        )
      )
    }
  }

  val component = ReactComponentB[Props]("EditorMultipleRows")
    .initialState_P(P ⇒ State(Seq.empty, showCreate = false, P.base.cachedDataF.currentValueU))
    .renderBackend[Backend]
    .configure(ComponentUpdates.inferred("EditorMultipleRows"))
    .componentDidMount(_.backend.init)
    .build

  def apply(l: StrLinkedRows) =
    component.withKey(l.fromCol.toString + "_" + l.toCol.toString + l.rows.size)
}

