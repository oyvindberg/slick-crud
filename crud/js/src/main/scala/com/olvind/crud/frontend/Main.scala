package com.olvind.crud
package frontend

import autowire._
import diode.react.{ModelProxy, ReactConnectProxy}
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
      val circuit: CrudCircuit =
        CrudCircuit(Model.init(UserInfo("Arne"), editors))

      val connector: ReactConnectProxy[Model] =
        circuit.connect(circuit.zoom(identity))

      def controller(p: Route, ctl: RouterCtl[Route]): ReactElement =
        connector((model: ModelProxy[Model]) ⇒ EditorController(model, circuit.sendCb, p)(ctl))

      import dsl._

      val editor: RouteB[EditorId] =
        string(editors.map(_.editorId.value).mkString("|"))
          .xmap[EditorId](EditorId)(_.value)

      val rowId: RouteB[StrRowId] =
        string("\\w+").xmap(StrRowId)(_.value)

      val oneRow: dsl.Rule =
        dynamicRouteCT("#" / (editor / "row" / rowId).caseClass[RouteEditorRow]) ~>
          dynRenderR(controller)

      val createRow: dsl.Rule =
        dynamicRouteCT("#" / (editor / "create").caseClass[RouteCreateRow]) ~>
          dynRenderR(controller)

      val allRows: Rule =
        dynamicRouteCT("#" / (editor / "read").caseClass[RouteEditor]) ~>
          dynRenderR(controller)

      val chooseEditor: Rule =
        staticRoute("#/", RouteChooseEditor) ~> renderR(ctl ⇒ controller(RouteChooseEditor, ctl))

      (oneRow | createRow | allRows | chooseEditor) notFound
        redirectToPage(RouteChooseEditor)(Redirect.Replace)
    }

  Styles.load()

  @JSExport
  override def main(): Unit =
    AjaxCall("editors")[Editors].editorDescs().call().onComplete{
      case Success(editors) ⇒
        val router = Router(baseUrl, routerConfig(editors).logToConsole)()
        ReactDOM.render(router, dom.document.getElementById("app"))
      case Failure(th) ⇒
        th.printStackTrace()
        dom.window.alert(s"Failed to start application: ${th.getMessage}")
    }
}
