package com.olvind.crud
package frontend

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import scalacss.internal.{FontFace, NonEmptyVector}
import scalacss.internal.mutable.GlobalRegistry

object Styles extends StyleSheet.Inline {
  import dsl._

  val body = style(
    fontFamily(FontFace("'Roboto', sans-serif", src = NonEmptyVector("https://fonts.googleapis.com/css?family=Roboto:400,300,500"))),
    fontSize(13.px),
    lineHeight(20.px)
  )

  def load(): Unit = {
    GlobalRegistry.register(
      this,
      EditorController.Style,
      TableStyle
    )

    GlobalRegistry.addToDocumentOnRegistration()
  }
}
