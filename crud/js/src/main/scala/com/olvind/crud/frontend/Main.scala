package com.olvind.crud
package frontend

import autowire._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.StaticDsl.RouteB
import japgolly.scalajs.react.extra.router._
import org.scalajs.dom

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scala.util.{Failure, Success}

object Main extends JSApp {
  val baseUrl = BaseUrl(dom.window.location.href.takeWhile(_ != '#'))

  def routerConfig(editors: Seq[EditorDesc]): RouterConfig[Route] =
    RouterConfigDsl[Route].buildConfig { dsl =>
      def edPage(p: Route, ctl: RouterCtl[Route]) =
        EditorController(editors.map(RouteEditor), p)(ctl)

      import dsl._

      val table: RouteB[EditorDesc] =
        string(editors.map(_.editorId.value).mkString("|"))
        .xmap[EditorDesc](s ⇒ editors.find(_.editorId.value =:= s).get)(_.editorId.value)

      val id: RouteB[StrRowId] =
        string("\\w+").xmap(StrRowId)(_.value)

      val oneRow: dsl.Rule =
        dynamicRouteCT("#" / (table / "row" / id).caseClass[RouteEditorRow]) ~>
          dynRenderR(edPage)

      val createRow: dsl.Rule =
        dynamicRouteCT("#" / (table / "create").caseClass[RouteCreateRow]) ~>
          dynRenderR(edPage)

      val allRows: Rule =
        dynamicRouteCT("#" / (table / "read").caseClass[RouteEditor]) ~>
          dynRenderR(edPage)

      val chooseEditor: Rule =
        staticRoute("#/", RouteChooseEditor) ~> renderR(ctl ⇒ edPage(RouteChooseEditor, ctl))

      (oneRow | createRow | allRows | chooseEditor) notFound
        redirectToPage(RouteChooseEditor)(Redirect.Replace)
    }

  Styles.load()

  @JSExport
  override def main() =
    AjaxCall("editors")[Editors].editorDescs().call().onComplete{
      case Success(editors) ⇒
        val router = Router(baseUrl, routerConfig(editors).logToConsole)()
        ReactDOM.render(router, dom.document.getElementById("app"))
      case Failure(th) ⇒
        th.printStackTrace()
        dom.window.alert(s"Failed to start application: ${th.getMessage}")
    }
}
