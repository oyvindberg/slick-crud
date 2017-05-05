package com.olvind.crud
package frontend

import autowire._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js
import scalacss.ScalaCssReact._

object EditorCreateRow extends EditorBase {

  override final type Data = Map[ColumnRef, U[StrValue]]

  case class Props(
    base:          EditorBaseProps,
    wasLinkedU:    U[StrLinkedRows],
    linkedViaValU: U[StrValue],
    onCreateU:     U[Callback]
  ) extends PropsB {

    def cols: List[ColumnDesc] =
      editorDesc.mainCols.filterNot(_.ref.isAutoInc)

    def initialValues: Map[ColumnRef, U[StrValue]] =
      cols.map(_.ref).map{
        col ⇒
          val existingU = for {
            wasLinked           ←  wasLinkedU
            if wasLinked.toCol =:= col
            linkedViaVal        ←  linkedViaValU
          } yield linkedViaVal
          col → existingU
      }.toMap
  }

  case class State(
    validationFails: Map[Option[StrRowId], Seq[ValidationError]],
    cachedDataOpt:   Option[CachedData],
    values:          Map[ColumnRef, U[StrValue]],
    created:         U[Option[StrRowId]]) extends StateB[State]{

    override def withCachedData(cd: CachedData): State =
      copy(cachedDataOpt = cd.some)

    override def withValidationFails(rowOpt: Option[StrRowId], ves: Seq[ValidationError]): State =
      copy(validationFails = validationFails.updated(rowOpt, ves))
  }

  private final case class Backend($: BackendScope[Props, State]) extends BackendB[Props, State]{

    override implicit val r: Reusability[Props] =
      ComponentUpdates.InferredReusability[Props]

    override val reInit: Callback =
      $.modState(_.copy(values = $.props.runNow().initialValues))

    def setColValue(c: ColumnRef)(str: StrValue): Callback =
      $.modState(s ⇒ s.copy(values = s.values.updated(c, str)))

    def trySave(P: Props, values: Map[ColumnRef, U[StrValue]]): Callback = {
      val vs: Map[ColumnRef, StrValue] =
        values mapValues (_ getOrElse StrValue(""))

      async((user, remote) => remote.create(user, vs).call(), "Create row".some){
        case XSuccess(idOpt) =>
          P.onCreateU getOrElse $.modState(_.copy(created = idOpt))
        case XValidation(_, fails) =>
          $.modState(_.withAddedValidationFails(None, fails))
      }
    }

    def renderSuccess(table: EditorDesc,
                      ctl:   RouterCtl[Route],
                      oid:   Option[StrRowId]): ReactElement = {
      val (title: String, targetPage: Route) =
        oid match {
          case Some(id) ⇒
            (s"View ${id.value}", RouteEditorRow(table, id))
          case None ⇒
            (s"View table",       RouteEditor(table))
        }

      <.div(
        <.h2("Successfully created new row"),
        Button(title, ctl.setEH(targetPage), Button.Primary)
      )
    }

    def renderForm(P: Props, S: State): ReactElement = {
      <.div(
        TableStyle.table,
        P.cols.map{
          col ⇒
            val errorOpt: U[ValidationError] =
              S.validationFails.get(None).flatMap(_.collectFirst {
                case ve@(col.ref, _) => ve
              }).asUndef

            <.div(
              TableStyle.row,
              TableHeaderCell(TableHeaderCell.Props(
                col,
                P.editorDesc.mainTable,
                js.undefined
              )),
              TableCell.createMode(
                clearError    = clearValidationFail(None)(col.ref),
                cachedDataOpt = S.cachedDataOpt,
                updateU       = setColValue(col.ref),
                col           = col,
                valueU        = S.values(col.ref),
                errorU        = errorOpt.asUndef,
                inputEnabled  = P.wasLinkedU.map(_.toCol) =/= col.ref
              )
            )
        }
      )
    }

    def render(P: Props, S: State): ReactElement = {
      <.div(
        TableStyle.centered,
        <.div(TableStyle.container)(
          EditorToolbar(EditorToolbar.Props(
            editorDesc        = P.editorDesc,
            rows              = 0,
            cachedDataOpt     = S.cachedDataOpt,
            filterU           = js.undefined,
            openFilterDialogU = js.undefined,
            isLinkedU         = P.wasLinkedU,
            refreshU          = reInit,
            showAllU          = fromProps.value().showAllRows,
            deleteU           = js.undefined,
            showCreateU       = js.undefined,
            customElemU       = Button("Save", (e: ReactEvent) ⇒ trySave(P, S.values), Button.Primary)
          )),
          S.created.toOption match {
            case Some(oid) ⇒ renderSuccess(P.editorDesc, P.base.ctl, oid)
            case None      ⇒ renderForm(P, S)
          }
        )
      )
    }
  }

  private val component =
    ReactComponentB[Props]("EditorCreateRow")
    .initialState_P(P ⇒
      State(
        Map.empty,
        P.base.cachedDataOpt,
        P.initialValues,
        created = js.undefined
      )
    )
    .renderBackend[Backend]
    .componentDidMount(_.backend.init)
    .configure(ComponentUpdates.inferred("EditorCreateRow"))
    .build

  def apply(p: Props): ReactElement =
    component.withKey(p.base.editorDesc.editorId.value)(p)
}
