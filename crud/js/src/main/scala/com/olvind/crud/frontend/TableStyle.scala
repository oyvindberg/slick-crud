package com.olvind.crud.frontend

import scalacss.Defaults._

object TableStyle extends StyleSheet.Inline {
  import dsl._

  val container = style(
    width(100.%%),
    display.flex,
    flexDirection.column,
    justifyContent.spaceAround,
    paddingTop(10.px)
  )

  val table = style(
    width(100.%%),
    boxShadow := "0 1px 3px 0 rgba(0, 0, 0, 0.12), 0 1px 2px 0 rgba(0, 0, 0, 0.24)",
    unsafeChild("div")(
      flex := "1"
    ),
    media.maxWidth(740.px)(
      boxShadow := "none"
    )
  )

  val nested = style(
    fontWeight.lighter,
    fontSize(95.%%),
    paddingLeft(2.%%)
  )

  val row = style(
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

  val headerRow = style(
    row,
    borderBottom :=! "1px solid #e0e0e0"
  )

  val cell = style(
    flex := "3 1",
    width(90.%%)
  )

  val headerCell = style(
    cell,
    fontWeight.bold,
    padding :=! "0.8rem"
  )

  val headerCellSortIcon = styleF.bool(ascending ⇒ styleS(
    &.after(
      fontSize(9.px),
      marginLeft(5.px),
      if (ascending) content := "'\\25B2'"
      else content := "'\\25BC'"
    )
  ))

  val settingsBar = style(
    display.flex,
    margin(15.px, `0`),
    justifyContent.spaceBetween
  )

  val unimportant = style(
    color.darkgray,
    fontWeight.lighter
  )

  val centered = style(
    display.flex,
    justifyContent.center
  )

  val error = style(
    color(c"#f44336")
  )
}
