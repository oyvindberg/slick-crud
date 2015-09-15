package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object forColumns{
  def apply[T](t: EditorDesc, ur: U[StrTableRow], ves: Seq[ValidationError])
              (f: (EditorDesc, ColumnDesc, U[StrRowId], U[StrValue], U[ErrorMsg]) ⇒ T) =
  t.columns.zipWithIndex.map{
    case (col, idx) ⇒
      val Oid = ur.toOption.flatMap(_.idOpt)
      val uv  = ur.map(_.values(idx))
      val ue = ves.collectFirst{
        case ValidationError(Oid, col.ref, e) => e
      }.asUndef
      f(t, col, Oid.asUndef, uv, ue)
  }
}

object TableHeaderCell {
  case class Props(
    col:       ColumnDesc,
    mainTable: TableName,
    sortedU:   U[SortOrder]){

    def otherTable: U[TableName] =
      col.ref.table.undef.filterNot(_ =:= mainTable)
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

  def apply(col: ColumnDesc) = component.withKey("header" + col.name.value)
}

object TableHeader {
  case class Props (
    editorDesc: EditorDesc,
    sortingU:   U[(ColumnRef, SortOrder)],
    onSort:     U[ColumnRef ⇒ Callback]
  )

  private val component = ReactComponentB[Props]("TableHeader")
    .render_P{P ⇒
      <.div(
        TableStyle.headerRow,
        P.editorDesc.columns.map(col ⇒
          <.div(
            TableHeaderCell(col)(TableHeaderCell.Props(
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

  def apply() = component
}

object TableRow {
  case class Props(
    editorDesc:      EditorDesc,
    row:             StrTableRow,
    cachedDataU:     U[CachedData],
    onUpdateU:       U[ColumnRef ⇒ StrValue ⇒ Callback],
    showSingleRow:   RouterCtl[StrRowId],
    validationFails: Seq[ValidationError],
    clearError:      ColumnRef => Callback
  )

  val component = ReactComponentB[Props]("TableRow")
    .render_P{P =>
      <.div(
        TableStyle.row,
        forColumns(P.editorDesc, P.row, P.validationFails){
          TableCell(P.clearError, P.cachedDataU, P.onUpdateU, P.showSingleRow)
        }
      )
    }
    .configure(ComponentUpdates.inferredNoState("TableRow"))
    .build

  def apply(row: StrTableRow) =
    component.withKey(row.idOpt.fold("")(_.value))
}