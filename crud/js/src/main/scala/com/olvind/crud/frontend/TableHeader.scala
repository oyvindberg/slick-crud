package com.olvind.crud
package frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Reusability, ReusableFn}
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object TableHeader {
  final case class Props (
    editor:     EditorDesc,
    sortingOpt: Option[(ColumnRef, SortOrder)],
    onSortOpt:  Option[ReusableFn[ColumnRef, Callback]]
  )

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.caseClass[Props]

  private final case class Backend($: BackendScope[Props, Unit]){
    def render(P: Props): ReactElement =
      <.div(
        TableStyle.headerRow,
        P.editor.columns.map(col ⇒
          <.div(
            TableHeaderCell(TableHeaderCell.Props(
              col,
              P.editor.mainTable,
              P.sortingOpt.collect{case (col.`ref`, order) ⇒ order}
            )),
            ^.cursor := "pointer",
            P.onSortOpt.map(onSort ⇒ ^.onClick --> onSort(col.ref))
          )
        )
      )
  }

  private val component =
    ReactComponentB[Props]("TableHeader")
      .renderBackend[Backend]
      .configure(ShouldUpdate.apply)
      .build

  def apply(p: Props): ReactElement =
    component(p)
}
