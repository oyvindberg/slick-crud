package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object TableHeaderCell {
  case class Props(col:       ColumnDesc,
                   mainTable: TableName,
                   sortedOpt: Option[SortOrder]){

    def otherTable: Option[TableName] =
      Some(col.ref.table).filterNot(_ =:= mainTable)
  }

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.caseClass[Props]

  private val component =
    ReactComponentB[Props]("TableHeaderCell")
      .render_P(P ⇒
        <.div(
          TableStyle.headerCell,
          P.col.name.value.split("_").map(_.capitalize).mkString(" "),
          " ",
          P.otherTable.map(name ⇒ <.span(TableStyle.unimportant, name.value)),
          P.sortedOpt.map(order ⇒ TableStyle.headerCellSortIcon(order == Asc))
        )
      ).configure(ShouldUpdate.apply)
      .build

  def apply(p: Props): ReactElement =
    component.withKey("header" + p.col.name.value)(p)
}
