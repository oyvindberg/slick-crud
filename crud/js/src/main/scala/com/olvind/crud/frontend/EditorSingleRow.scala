package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Reusability, ReusableFn}
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object EditorSingleRow {

  final case class Props(
    sendAction:    ReusableFn[Action, Callback],
    model:         EditorModel,
    cachedDataOpt: Option[CachedData],
    rowId:         StrRowId,
    renderLinked:  (StrLinkedRows, Map[ColumnRef, StrValue]) ⇒ ReactNode
  )

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.caseClass[Props]

  private final case class Backend($: BackendScope[Props, Unit]) {
    val loadData: Callback =
      $.props.flatMap(
        P => P.sendAction(FetchRow(P.model.editor.editorId, P.rowId))
      )
    val showAllRows: Callback =
      $.props.flatMap(
        P => P.sendAction(Navigate(RouteEditor(P.model.editor.editorId)))
      )
    val deleteRow: Callback =
      $.props.flatMap(
        P => P.sendAction(DeleteRow(P.model.editor.editorId, P.rowId))
      )
    val showCreateRow: Callback =
      $.props.flatMap(
        P => P.sendAction(Navigate(RouteCreateRow(P.model.editor.editorId)))
      )

    def render(P: Props): ReactElement = {
      WaitingRow(P.model.rows.flatMap(_.headOption)){
        (row: StrTableRow) ⇒

          val valuesByCol: Map[ColumnRef, StrValue] =
            (P.model.editor.columns.map(_.ref) zip row.values).toMap

          <.div(TableStyle.container)(
            EditorToolbar(EditorToolbar.Props(
              editorDesc          = P.model.editor,
              rows                = 1,
              cachedDataOpt       = P.cachedDataOpt,
              refresh             = loadData,
              showAll             = showAllRows,
              deleteOpt           = Some(deleteRow),
              showCreateOpt       = Some(showCreateRow)
            )),

            renderRow(P, row),

            <.div(
              TableStyle.nested,
              (row.idOpt flatMap P.model.linkedRows.get).map(
                _.map {
                  (linkedRows: StrLinkedRows) ⇒
                    val prefilled: Map[ColumnRef, StrValue] =
                      valuesByCol.get(linkedRows.fromCol) match {
                        case None => Map.empty
                        case Some(value) => Map(linkedRows.toCol -> value)
                      }
                    println(prefilled)
                    P.renderLinked(linkedRows, prefilled)
                }
              )
            )
          )
      }
    }

    def renderRow(P: Props, row: StrTableRow): ReactElement =
      <.div(
        TableStyle.table,
        row.values.zip(P.model.editor.columns).map{
          case (value, col) ⇒
            val validationErrorOpt: Option[ValidationError] =
              P.model.validationErrors.get(row.idOpt).flatMap(_.get(col.ref))

            val onUpdateOpt: Option[StrValue ⇒ Callback] =
              row.idOpt.map(rowId ⇒ (value: StrValue) ⇒
                P.sendAction(UpdateValue(P.model.editor.editorId, rowId, col.ref, value))
              )

            <.div(
              TableStyle.row,
              TableHeaderCell(TableHeaderCell.Props(
                col,
                P.model.editor.mainTable,
                None
              )),
              TableCell(
                sendAction         = P.sendAction,
                editorDesc         = P.model.editor,
                col                = col,
                rowIdOpt           = row.idOpt,
                cachedDataOpt      = P.cachedDataOpt,
                valueOpt           = Some(value),
                validationErrorOpt = validationErrorOpt,
                onUpdateOpt        = onUpdateOpt
              )
            )
        }
      )
  }

  private val component =
    ReactComponentB[Props]("EditorSingleRow")
      .stateless
      .renderBackend[Backend]
      .configure(ShouldUpdate.apply)
      .componentDidMount(_.backend.loadData)
      .build

  def apply(p: Props): ReactElement =
    component.withKey(p.model.editor.editorId.value + p.rowId.value)(p)
}
