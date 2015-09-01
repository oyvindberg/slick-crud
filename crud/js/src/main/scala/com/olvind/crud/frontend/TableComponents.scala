package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object forColumns{
  def apply[T](t: ClientTable, ur: U[StrTableRow])
              (f: (ClientTable, ClientColumn, U[StrRowId], U[StrValue]) ⇒ T) =
  t.columns.zipWithIndex.map{
    case (col, idx) ⇒
      val oid = ur.flatMap(_.idOpt.asUndef)
      val ov  = ur.map(_.values(idx))
      f(t, col, oid, ov)
  }
}

object TableHeaderCell {
  case class Props(
    col:       ClientColumn,
    mainTable: TableName,
    sortedU:   U[SortOrder]){

    def otherTable: U[TableName] =
      col.column.table.undef.filterNot(_ =:= mainTable)
  }

  val component = ReactComponentB[Props]("TableHeaderCell").render_P(P ⇒
    <.div(
      TableStyle.headerCell,
      P.col.name.value.split("_").map(_.capitalize).mkString(" "),
      " ",
      P.otherTable.map(name ⇒ <.span(TableStyle.unimportant, name.value)),
      P.sortedU.map(order ⇒ TableStyle.headerCellSortIcon(order == Asc))
    )
  ).configure(ComponentUpdates.inferredNoState("TableHeaderCell"))
   .build

  def apply(col: ClientColumn) = component.withKey("header" + col.name.value)
}

object TableHeader {
  case class Props (
    table:    ClientTable,
    sortingU: U[(ColumnInfo, SortOrder)],
    onSort:   U[ColumnInfo ⇒ Callback]
  )

  private val component = ReactComponentB[Props]("TableHeader")
    .render_P{P ⇒
      <.div(
        TableStyle.headerRow,
        P.table.columns.map(col ⇒
          <.div(
            TableHeaderCell(col)(TableHeaderCell.Props(col, P.table.name, P.sortingU.collect{case (col.column, order) ⇒ order})),
            ^.cursor := "pointer",
            ^.onClick -->? P.onSort.mapply(col.column)
          )
        )
      )
    }.configure(ComponentUpdates.inferredNoState("TableHeader"))
     .build

  def apply() = component
}

object TableRow {
  case class Props (
    table:         ClientTable,
    row:           StrTableRow,
    cachedDataU:   U[CachedData],
    onUpdateU:     U[ColumnInfo ⇒ StrValue ⇒ Callback],
    showSingleRow: RouterCtl[StrRowId]
  )

  val component = ReactComponentB[Props]("TableRow")
    .render_P{P =>
      <.div(
        TableStyle.row,
        forColumns(P.table, P.row){
          TableCell(P.cachedDataU, P.onUpdateU, P.showSingleRow)
        }
      )
    }
    .configure(ComponentUpdates.inferredNoState("TableRow"))
    .build

  def apply(row: StrTableRow) =
    component.withKey(row.idOpt.fold("")(_.value))
}