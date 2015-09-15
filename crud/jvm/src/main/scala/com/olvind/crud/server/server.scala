package com.olvind.crud

import slick.lifted.{FlatShapeLevel, Rep, Shape}

package object server {
  /* export types so clients can do a import server._ */
  val CellRendering = _root_.com.olvind.crud.CellRendering
  type CellRendering = _root_.com.olvind.crud.CellRendering
  type Editor = _root_.com.olvind.crud.Editor
  type EditorName = _root_.com.olvind.crud.EditorName

  /* A suitably long number for a dropdown*/
  private[server] val maxNumLinks = 300

  /* we need to prove this quite a few places, so avoid repeating ourselves */
  private[server] type FlatRepShape[T] = Shape[FlatShapeLevel, Rep[T], T, Rep[T]]
  
  /* package level import */
  private[server] type AbstractTable[T] = slick.lifted.AbstractTable[T]
}
