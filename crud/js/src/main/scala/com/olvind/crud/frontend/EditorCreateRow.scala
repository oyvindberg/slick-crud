package com.olvind.crud
package frontend

import autowire._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._

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
    validationFails: Seq[ValidationError],
    cachedDataU:     U[CachedData],
    values:          Map[ColumnRef, U[StrValue]],
    created:         U[Option[StrRowId]]) extends StateB[State]{

    override def withCachedData(cd: CachedData) =
      copy(cachedDataU = cd)

    override def withValidationFails(ves: Seq[ValidationError]) =
      copy(validationFails = ves)
  }

  final case class Backend($: WrapBackendScope[Props, State]) extends BackendB[Props, State]{

    override def reInit = $.modState(_.copy(values = $.props.initialValues))

    def handleFailed(f: CreateFailed): Callback =
      f.ve match {
        case -\/(error) =>
          criticalError(f)
        case  \/-(errors) =>
          $.modState(_.withAddedValidationFails(errors.map{
            case (c, e) => ValidationError(None, c, e)
          }))
      }

    def setColValue(c: ColumnRef)(str: StrValue): Callback =
      $.modState(s ⇒ s.copy(values = s.values.updated(c, str)))

    def trySave(values: Map[ColumnRef, U[StrValue]]): Callback = {
      val vs = values mapValues (_ getOrElse StrValue(""))
      asyncCb.applyEither("Couldn't create new row", remote.create($.props.base.userInfo, vs).call())(handleFailed)
        .commit(created ⇒ $.props.onCreateU getOrElse $.modState(_.copy(created = created.oid)))
    }

    def renderSuccess(table: EditorDesc,
                      ctl:   RouterCtl[Route],
                      oid:   Option[StrRowId]): ReactElement = {
      val (title, targetPage) = oid match {
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
            val errorOpt = S.validationFails.collectFirst {
              case ValidationError(None, col.ref, e) => e
            }
            <.div(
              TableStyle.row,
              TableHeaderCell(col)(TableHeaderCell.Props(
                col,
                P.editorDesc.mainTable,
                uNone
              )),
              TableCell.createMode(
                clearError   = clearValidationFail(None)(col.ref),
                cachedDataU  = S.cachedDataU,
                updateU      = setColValue(col.ref),
                col          = col,
                valueU       = S.values(col.ref),
                errorU       = errorOpt.asUndef,
                inputEnabled = P.wasLinkedU.map(_.toCol) =/= col.ref
              )
            )
        }
      )
    }

    def render(P: Props, S: State): ReactElement = {
      <.div(
        TableStyle.centered,
        <.div(TableStyle.container)(
          EditorToolbar()(EditorToolbar.Props(
            editorDesc             = $.props.editorDesc,
            rows              = 0,
            cachedDataU       = S.cachedDataU,
            filterU           = uNone,
            openFilterDialogU = uNone,
            isLinkedU         = P.wasLinkedU,
            refreshU          = reInit,
            showAllU          = showAllRows,
            deleteU           = uNone,
            showCreateU       = uNone,
            customElemU       = Button("Save", (e: ReactEvent) ⇒ trySave(S.values), Button.Primary)
          )),
          S.created.toOption match {
            case Some(oid) ⇒ renderSuccess(P.editorDesc, P.base.ctl, oid)
            case None      ⇒ renderForm(P, S)
          }
        )
      )
    }
  }

  val component = ReactComponentB[Props]("EditorCreateRow")
    .initialState_P(P ⇒
      State(
        Seq.empty,
        P.base.cachedDataF.currentValueU,
        P.initialValues,
        created = uNone
      )
    )
    .backend($ ⇒ Backend(WrapBackendScope($)))
    .render($ ⇒ $.backend.render($.props, $.state))
    .componentDidMount(_.backend.init)
    .configure(ComponentUpdates.inferred("EditorCreateRow"))
    .build

  def apply(editorDesc: EditorDesc) =
    component.withKey(editorDesc.editorId.value)
}
