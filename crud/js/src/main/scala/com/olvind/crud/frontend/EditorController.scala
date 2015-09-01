package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.RefHolder
import chandu0101.scalajs.react.components.materialui.{MuiAppBar, MuiDrawer, MuiDrawerM, MuiMenuItem, MuiMuiThemeProvider}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.concurrent.Future
import scala.scalajs.js
import scala.util.{Failure, Success}
import scalacss.Defaults._
import scalacss.ScalaCssReact._

object EditorController {

  case class Props(
    menu:         Seq[(EditorName, RouteEditor)],
    current:      Route,
    ctl:          RouterCtl[Route],
    lookupLinked: EditorId ⇒ Option[EditorDesc]
  )
  implicit val r0 = ComponentUpdates.InferredReusability[Props]
  
  case class State(
    menuOpen: Boolean,
    res: List[TimedRes]
  )

  val userInfo = UserInfo("arne") //todo

  private final case class Backend($: BackendScope[Props, State]) extends TimerSupport {
    val onResult: TimedRes ~=> Callback =
      ReusableFn {
        case (t: TimedRes) ⇒
          $.modState(s ⇒ s.copy(res = t +: s.res)) >>
          setTimeout($.modState(s ⇒ s.copy(res = s.res.filterNot(_ =:= t))), UpdateNotifierClient.ttl)
      }
    
    val fromProps: Px[FromProps] =
      Px.cbA($.props).map(new FromProps(_))

    val leftNavRef = RefHolder[MuiDrawerM]

    final class FromProps(P: Props){
      
      object currentTable {
        def index: U[Int] = 
          P.menu.indexWhere(_._2.t.some =:= opt).uSome.filterNot(_ =:= -1)
  
        def opt: Option[EditorDesc] = P.current.some collect {
          case RouteEditor(t) ⇒ t
        }
      }
      
      object nav {
        val toggle: Callback =
          $.modState(s => s.copy(menuOpen = !s.menuOpen))

        val menuItems: js.Array[ReactElement] =
          P.menu.toJsArray.map{
            case (name, r: RouteEditor) ⇒
              MuiMenuItem(
                key         = name.value,
                primaryText = name.value,
                value       = r.t.mainTable.value,
                onTouchTap  = (e: ReactTouchEventH) => (P.ctl set r) >> toggle
              )()
          }

        val toggleButton: ReactElement =
          Button("Editors", (e: ReactEvent) ⇒ toggle, Button.Primary)
      }

      def currentValue[T](f: Future[T]): Option[T] =
        f.value.flatMap {
          case Failure(_) => None
          case Success(t) => Some(t)
        }

      def baseProps(t: EditorDesc): EditorBaseProps =
        EditorBaseProps(
          userInfo      = userInfo,
          editorDesc    = t,
          cachedDataOpt = currentValue(Cache(userInfo, t)),
          ctl           = P.ctl,
          onResult      = onResult
        )

      def renderLinked(reload:  Callback,
                       linked:  StrLinkedRows,
                       viaValU: U[StrValue]): ReactNode = {
        P.lookupLinked(linked.editorId) match {
          case Some(desc) ⇒
            val create =
              EditorCreateRow(EditorCreateRow.Props(
                baseProps(desc), linked, viaValU, reload
              ))

            <.div(
              linked.rows.toList match {
                case Nil ⇒
                  create
                case row :: Nil ⇒
                  EditorLinkedSingleRow(
                    EditorLinkedSingleRow.Props(
                      baseProps(desc), linked, reload, create
                    )
                  )
                case rows ⇒
                  EditorLinkedMultipleRows(
                    EditorLinkedMultipleRows.Props(
                      baseProps(desc), linked, reload, create
                    )
                  )
              }
            )
          case None ⇒
            <.div(s"Editor ${linked.editorId} is not exported")
        }
      }
    }

    def render(P: Props, S: State): ReactElement = {
      val fp = fromProps.value()
      MuiMuiThemeProvider()(
        <.div(
          Styles.body,
          MuiAppBar(
            title              = s"Slick-Crud${fp.currentTable.opt.fold("")(t ⇒ s": ${t.title}")}",
            iconElementLeft    = fp.nav.toggleButton,
            showMenuIconButton = true
          )(),
          MuiDrawer(
            docked          = false,
            ref             = leftNavRef.set,
            open            = S.menuOpen,
            onRequestChange = (open: Boolean, reason: String) => $.modState(_.copy(menuOpen = open))
          )(fp.nav.menuItems),
          <.div(
            <.div(Style.container,
              P.current match {
                case RouteChooseEditor         ⇒
                  <.h2("Choose an editor")

                case RouteEditor(t)        ⇒
                  EditorMultipleRows(EditorMultipleRows.Props(
                    fp.baseProps(t)
                  ))

                case RouteEditorRow(t, rowId) ⇒
                  EditorSingleRow(EditorSingleRow.Props(
                    fp.baseProps(t),
                    rowId        = rowId,
                    renderLinked = fp.renderLinked
                  ))

                case RouteCreateRow(t) ⇒
                  EditorCreateRow(EditorCreateRow.Props(
                    fp.baseProps(t),
                    wasLinkedU    = js.undefined,
                    linkedViaValU = js.undefined,
                    onCreateU     = js.undefined
                  ))
              }
            ),
            UpdateNotifierClient(S.res)
          )
        )
      )
    }
  }

  private val component =
    ReactComponentB[Props]("EditorController")
      .initialState(State(true, Nil))
      .renderBackend[Backend]
      .configure(ComponentUpdates.inferred("EditorController"))
      .build

  def apply(menu:        Seq[RouteEditor],
            currentPage: Route)
           (ctrl:        RouterCtl[Route]): ReactElement = {

    val namedEditors: Seq[(EditorName, RouteEditor)] =
      menu.map {
        case r@RouteEditor(EditorDesc(name, _, _, _, _, _)) => name -> r
      }

    val lookupId: EditorId ⇒ Option[EditorDesc] =
      Id ⇒ menu collectFirst {
        case RouteEditor(d@EditorDesc(_, Id, _, _, _, _)) => d
      }

    component(Props(namedEditors, currentPage, ctrl, lookupId))
  }


  object Style extends StyleSheet.Inline {
    import dsl._
    
    val container = style(
      padding(20.px),
      borderLeft(c"rgb(223, 220, 220)", 1.px, solid),
      flex := "1"
    )
  }
}
