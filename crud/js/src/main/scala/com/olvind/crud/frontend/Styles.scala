package com.olvind.crud
package frontend

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import scalacss.ext.CssReset
import scalacss.mutable.{GlobalRegistry, StyleSheet}

object Styles extends StyleSheet.Inline {
  import dsl._

  val body = style(
    CssReset.normaliseCss,
    fontFamily := "'Roboto', sans-serif",
    fontSize(13.px),
    lineHeight(20.px)
  )

  def load() = {
    GlobalRegistry.register(
      this,
      EditorController.Style,
      TableStyle
    )
    GlobalRegistry.onRegistration(_.addToDocument())
  }
}
