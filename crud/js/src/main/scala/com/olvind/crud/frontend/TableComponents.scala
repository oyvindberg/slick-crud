package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object forColumns{
  def apply[T](t: EditorDesc, row: StrTableRow, ves: Map[Option[StrRowId], Seq[ValidationError]])
              (f: (EditorDesc, ColumnDesc, U[StrRowId], U[StrValue], U[ValidationError]) ⇒ T) =

    t.columns.zipWithIndex.map{
      case (col, idx) ⇒
        val validationError: U[ValidationError] =
          ves.get(row.idOpt).flatMap(_.collectFirst{
            case ve@(col.ref, e) => ve
          }).asUndef

        f(t, col, row.idOpt.asUndef, row.values(idx), validationError)
    }
}

object TableHeaderCell {
  case class Props(
    col:       ColumnDesc,
    mainTable: TableName,
    sortedU:   U[SortOrder]){

    def otherTable: U[TableName] =
      col.ref.table.uSome.filterNot(_ =:= mainTable)
  }

  private val component =
    ReactComponentB[Props]("TableHeaderCell")
      .render_P(P ⇒
        <.div(
          TableStyle.headerCell,
          P.col.name.value.split("_").map(_.capitalize).mkString(" "),
          " ",
          P.otherTable.map(name ⇒ <.span(TableStyle.unimportant, name.value)),
          P.sortedU.map(order ⇒ TableStyle.headerCellSortIcon(order == Asc))
        )
      ).configure(ComponentUpdates.inferredNoState("TableHeaderCell"))
       .build

  def apply(p: Props): ReactElement =
    component.withKey("header" + p.col.name.value)(p)
}

object TableHeader {
  case class Props (
    editorDesc: EditorDesc,
    sortingU:   U[(ColumnRef, SortOrder)],
    onSort:     U[ColumnRef ⇒ Callback]
  )

  private val component =
    ReactComponentB[Props]("TableHeader")
    .render_P{P ⇒
      <.div(
        TableStyle.headerRow,
        P.editorDesc.columns.map(col ⇒
          <.div(
            TableHeaderCell(TableHeaderCell.Props(
              col,
              P.editorDesc.mainTable,
              P.sortingU.collect{case (col.`ref`, order) ⇒ order}
            )),
            ^.cursor := "pointer",
            ^.onClick -->? P.onSort.mapply(col.ref)
          )
        )
      )
    }.configure(ComponentUpdates.inferredNoState("TableHeader"))
     .build

  def apply(p: Props): ReactElement =
    component(p)
}

object TableRow {
  case class Props(
    editorDesc:      EditorDesc,
    row:             StrTableRow,
    cachedDataOpt:   Option[CachedData],
    onUpdateU:       U[ColumnRef ⇒ StrValue ⇒ Callback],
    showSingleRow:   RouterCtl[StrRowId],
    validationFails: Map[Option[StrRowId], Seq[ValidationError]],
    clearError:      ColumnRef => Callback
  )

  private val component =
    ReactComponentB[Props]("TableRow")
      .render_P { P =>
        <.div(
          TableStyle.row,
          forColumns(P.editorDesc, P.row, P.validationFails) {
            TableCell(P.clearError, P.cachedDataOpt, P.onUpdateU, P.showSingleRow)
          }
        )
      }
      .configure(ComponentUpdates.inferredNoState("TableRow"))
      .build

  def apply(p: Props): ReactElement =
    component.withKey(p.row.idOpt.fold("")(_.value))(p)
}