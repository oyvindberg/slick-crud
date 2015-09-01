package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js
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
    validationFails: Map[Option[StrRowId], Seq[ValidationError]],
    showCreate:      Boolean,
    cachedDataOpt:   Option[CachedData]) extends StateB[State]{

    override def withCachedData(cd: CachedData) =
      copy(cachedDataOpt = cd.some)

    override def withValidationFails(rowOpt: Option[StrRowId], ves: Seq[ValidationError]) =
      copy(validationFails = validationFails.updated(rowOpt, ves))
  }

  private final case class Backend($: BackendScope[Props, State])
    extends BackendBUL[Props, State]
    with OnUnmount {

    override implicit val r = ComponentUpdates.InferredReusability[Props]

    val toggleShowCreate: ReactEvent ⇒ Callback =
      e ⇒ $.modState(S ⇒ S.copy(showCreate = !S.showCreate))

    override def render(P: Props, S: State): ReactElement = {
      val fp = fromProps.value()

      <.div(
        TableStyle.container,
        EditorToolbar(EditorToolbar.Props(
          editorDesc        = P.editorDesc,
          rows              = P.rows.size,
          cachedDataOpt     = S.cachedDataOpt,
          filterU           = js.undefined,
          openFilterDialogU = js.undefined,
          isLinkedU         = P.linkedRows,
          refreshU          = reInit,
          showAllU          = fp.showAllRows,
          deleteU           = js.undefined,
          showCreateU       = (S.showCreate, toggleShowCreate),
          customElemU       = js.undefined
        )),
        <.div(
          TableStyle.table,
          <.div(
            TableStyle.nested,
            P.createElem.some.filter(_ ⇒ S.showCreate)
          ),
          TableHeader(TableHeader.Props(
            editorDesc = P.editorDesc,
            sortingU   = js.undefined,
            onSort     = js.undefined
          )),
          P.rows.map(
            row ⇒ TableRow(TableRow.Props(
              editorDesc      = P.editorDesc,
              row             = row,
              cachedDataOpt   = S.cachedDataOpt,
              onUpdateU       = row.idOpt.asUndef.map(updateValue),
              showSingleRow   = fp.showSingleRow,
              validationFails = S.validationFails,
              clearError      = clearValidationFail(row.idOpt)
            ))
          )
        )
      )
    }
  }

  private val component =
    ReactComponentB[Props]("EditorMultipleRows")
      .initialState_P(P ⇒ State(Map.empty, showCreate = false, P.base.cachedDataOpt))
      .renderBackend[Backend]
      .configure(ComponentUpdates.inferred("EditorMultipleRows"))
      .componentDidMount(_.backend.init)
      .build

  def apply(p: Props): ReactElement =
    component.withKey(s"${p.linkedRows.fromCol}_${p.linkedRows.toCol}" + p.linkedRows.rows.size)(p)

}

