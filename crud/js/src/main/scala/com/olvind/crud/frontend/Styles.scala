package com.olvind.crud
package frontend

import scalacss._
import scalacss.Defaults._
import scalacss.ScalaCssReact._

object Styles extends mutable.StyleSheet.Inline {
  import dsl._

  val body = style(
    fontFamily(FontFace("'Roboto', sans-serif", src = NonEmptyVector("https://fonts.googleapis.com/css?family=Roboto:400,300,500"))),
    fontSize(13.px),
    lineHeight(20.px)
  )

  def load(): Unit = {
    mutable.GlobalRegistry.register(
      this,
      EditorController.Style,
      TableStyle
    )
    mutable.GlobalRegistry.onRegistration(_.addToDocument())
  }
}
