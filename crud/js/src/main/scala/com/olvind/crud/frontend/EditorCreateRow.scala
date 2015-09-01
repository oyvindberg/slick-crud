package com.olvind.crud
package frontend

import autowire._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object EditorCreateRow extends EditorBase {

  override final type Data = Map[ColumnInfo, U[StrValue]]

  case class Props(
    base:          EditorBaseProps,
    wasLinkedU:    U[StrLinkedRows],
    linkedViaValU: U[StrValue],
    onCreateU:     U[Callback]
  ) extends PropsB {

    def cols: List[ClientColumn] =
      table.originalColumns.filterNot(_.column.isAutoInc)

    def initialValues: Map[ColumnInfo, U[StrValue]] =
      cols.map(_.column).map{
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
    cachedDataU: U[CachedData],
    values:      Map[ColumnInfo, U[StrValue]],
    failure:     U[CrudFailure],
    created:     U[Option[StrRowId]]) extends StateB[State]{

    override def withCachedData(cd: CachedData) =
      copy(cachedDataU = cd)
  }

  case class Backend($: WrapBackendScope[Props, State]) extends BackendB[Props, State]{

    override def reInit = $.modState(_.copy(values = $.props.initialValues))

    override def handleFailure(failure: CrudFailure): Callback =
      $.modState(_.copy(failure = failure))

    def setColValue(c: ColumnInfo)(str: StrValue): Callback =
      $.modState(s ⇒ s.copy(values = s.values.updated(c, str)))

    def trySave(values: Map[ColumnInfo, U[StrValue]]): Callback = {
      val vs = values mapValues (_ getOrElse StrValue(""))
      asyncCb("Couldn't create new row", remote.create($.props.base.userInfo, vs).call())
        .commit(created ⇒ $.props.onCreateU getOrElse $.modState(_.copy(created = created.oid)))
    }

    def renderSuccess(table: ClientTable,
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
            val errorU: U[String] = S.failure.collect {
              case CreateFailed(_, \/-(errors)) ⇒ errors.collectFirst {
                case (col.column, e) ⇒ e.value
              }.asUndef
            }.flatten

          <.div(
            TableStyle.row,
            TableHeaderCell(col)(TableHeaderCell.Props(
              col,
              P.table.name,
              uNone
            )),
            TableCell.createMode(
              cachedDataU  = S.cachedDataU,
              updateU      = setColValue(col.column),
              col          = col,
              valueU       = S.values(col.column),
              errorU       = errorU,
              inputEnabled = P.wasLinkedU.map(_.toCol) =/= col.column
            )
          )
        },
        S.failure.toOption.collect{
          case CreateFailed(_, -\/(error)) ⇒ error.value
        }
      )
    }

    def render(P: Props, S: State): ReactElement = {
      <.div(
        TableStyle.centered,
        <.div(TableStyle.container)(
          EditorToolbar()(EditorToolbar.Props(
            table             = $.props.table,
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
            case Some(oid) ⇒ renderSuccess(P.table, P.base.ctl, oid)
            case None      ⇒ renderForm(P, S)
          }
        )
      )
    }
  }

  val component = ReactComponentB[Props]("EditorCreateRow")
    .initialState_P(P ⇒
      State(
        P.base.cachedDataF.currentValueU,
        P.initialValues,
        failure = uNone,
        created = uNone
      )
    )
    .backend($ ⇒ Backend(WrapBackendScope($)))
    .render($ ⇒ $.backend.render($.props, $.state))
    .componentDidMount(_.backend.init)
    .configure(ComponentUpdates.inferred("EditorCreateRow"))
    .build

  def apply(t: ClientTable) =
    component.withKey(t.name.value)
}
