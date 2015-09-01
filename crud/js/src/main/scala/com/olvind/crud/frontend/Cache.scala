package com.olvind.crud
package frontend

import autowire._
import org.scalajs.dom

import scala.concurrent.Future

object Cache {
  private var cache: Map[EditorId, Future[CachedData]] = Map.empty

  private def fetch(u: UserInfo, t: EditorDesc): Future[CachedData] = {
    val remote = AjaxCall.forEditor(t.editorId)[Editor]
    val f      = remote.cachedData(u).call()
    f.onFailure {
      case th =>
        dom.window.console.warn(s"Could not load cached data for ${t.editorName}: ${th.getMessage}")
        cache - t.editorId
    }
    f
  }

  private def save(eid: EditorId)(fcd: Future[CachedData]): Unit =
    cache = cache.updated(eid, fcd)

  def apply(u: UserInfo, t: EditorDesc): Future[CachedData]  =
    cache getOrElse(t.editorId, fetch(u, t) <| save(t.editorId))
}
