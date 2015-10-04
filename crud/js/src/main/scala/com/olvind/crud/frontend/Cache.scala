package com.olvind.crud
package frontend

import autowire._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.ReusableFn

object Cache {
  var cachedDataVar = Map.empty[EditorId, CFuture[CachedData]]

  val nothing = ReusableFn((in: TimedT[CrudResult]) ⇒ Callback.empty)

  def cleanup(eid: EditorId)(fail: CrudFailure) = Callback {
    cachedDataVar = cachedDataVar - eid
  }

  def fetch(u: UserInfo, t: EditorDesc): CFuture[CachedData] = {
    val remote = AjaxCall.forEditor(t.editorId)[Editor]
    val async  = AsyncCallback(remote, u, nothing, cleanup(t.editorId), t.editorId)
    async("Could not load cached data", r => u => r.cachedData(u).call()).runNow().map(_.cd)
  }

  def save(eid: EditorId)(fcd: CFuture[CachedData]): Unit =
    cachedDataVar = cachedDataVar.updated(eid, fcd)

  def apply(u: UserInfo, t: EditorDesc): CFuture[CachedData]  =
    cachedDataVar getOrElse(t.editorId, fetch(u, t) <| save(t.editorId))

  def invalidate(): Unit =
    cachedDataVar = Map.empty
}
