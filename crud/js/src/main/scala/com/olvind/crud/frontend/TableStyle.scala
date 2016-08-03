package com.olvind.crud.frontend

import scalacss.Defaults._
import scalacss.StyleA

object TableStyle extends StyleSheet.Inline {
  import dsl._

  val container: StyleA =
    style(
      width(100.%%),
      display.flex,
      flexDirection.column,
      justifyContent.spaceAround,
      paddingTop(10.px)
    )

  val table: StyleA =
    style(
      width(100.%%),
      boxShadow := "0 1px 3px 0 rgba(0, 0, 0, 0.12), 0 1px 2px 0 rgba(0, 0, 0, 0.24)",
      unsafeChild("div")(
        flex := "1"
      ),
      media.maxWidth(740.px)(
        boxShadow := "none"
      )
    )

  val nested: StyleA =
    style(
      fontWeight.lighter,
      fontSize(95.%%),
      paddingLeft(2.%%)
    )

  val row: StyleA =
    style(
      display.flex,
      alignItems.center,
      flexDirection.row,
      padding :=! "0.4rem",
      &.hover(
        backgroundColor(c"rgba(244, 244, 244, 0.77)")
      ),
      media.maxWidth(740.px)(
        display.flex,
        flexDirection.column,
        boxShadow := "0 1px 3px grey",
        margin(5.px)
      )
    )

  val headerRow: StyleA =
    style(
      row,
      borderBottom :=! "1px solid #e0e0e0"
    )

  val cell: StyleA =
    style(
      flex := "3 1",
      width(90.%%)
    )

  val headerCell: StyleA =
    style(
      cell,
      fontWeight.bold,
      padding :=! "0.8rem"
    )

  val headerCellSortIcon: (Boolean) => StyleA =
    styleF.bool(ascending ⇒ styleS(
      &.after(
        fontSize(9.px),
        marginLeft(5.px),
        if (ascending) content := "'\\25B2'"
        else content := "'\\25BC'"
      )
    ))

  val settingsBar: StyleA =
    style(
      display.flex,
      margin(15.px, `0`),
      justifyContent.spaceBetween
    )

  val unimportant: StyleA =
    style(
      color.darkgray,
      fontWeight.lighter
    )

  val centered: StyleA =
    style(
      display.flex,
      justifyContent.center
    )

  val error: StyleA =
    style(
      color(c"#f44336")
    )
}
