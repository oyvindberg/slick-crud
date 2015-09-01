package com.olvind.crud.frontend

import japgolly.scalajs.react._

case class WrapBackendScope[P, S]($: BackendScope[P, S]){
  def props = $.props

  val state = CallbackTo[S]($.state)

  def setState(c: CallbackTo[S]): Callback =
    state >>= (s => $.setState(c.runNow()))

  def modState(f: S => S, cb: Callback = Callback.empty): Callback =
    state >>= (s => $.setState(f(s), cb))

  def modStateCB(f: S => CallbackTo[S], cb: Callback = Callback.empty): Callback =
    state >>= (s => $.setStateCB(f(s), cb))
}
