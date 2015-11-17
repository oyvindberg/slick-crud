package com.olvind.crud
package frontend

import autowire._
import chandu0101.scalajs.react.components.RefHolder
import chandu0101.scalajs.react.components.materialui._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js
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
    res: List[TimedT[CrudResult]]
  )

  val userInfo = UserInfo("arne") //todo

  case class Backend($: BackendScope[Props, State]) {
    val onResult: (TimedT[CrudResult]) ~=> Callback =
      ReusableFn {
        case (t: TimedT[CrudResult]) ⇒
          Callback {
            t.value match {
              case Created(_, _)| Deleted(_, _) ⇒ Cache.invalidate()
              case _                            ⇒ ()
            }

            $.modState(s ⇒ s.copy(res = t +: s.res)).runNow()

            js.timers.setTimeout(UpdateNotifierClient.ttl){
              $.modState(s ⇒ s.copy(res = s.res.filterNot(_ =:= t))).runNow()
            }
          }
      }
    
    val fromProps  = Px.cbA($.props).map(new FromProps(_))
    val leftNavRef = RefHolder[MuiLeftNavM]

    final class FromProps(P: Props){
      
      object currentTable {
        def index: U[Int] = 
          P.menu.indexWhere(_._2.t.some =:= opt).undef.filterNot(_ =:= -1)
  
        def opt: Option[EditorDesc] = P.current.some collect {
          case RouteEditor(t) ⇒ t
        }
      }
      
      object nav {
        val menuItems: js.Array[MuiMenuItemJson] =
          P.menu.toJsArray.map{
            case (name, r) ⇒ MuiMenuItemJson(text = name.value, payload = r.t.mainTable.value)
          }
  
        val toggle: Callback =
          leftNavRef().map(_.toggle())
  
        val onTableChosen: (ReactEvent, Int, js.Any) ⇒ Callback =
          (e, i, o) ⇒ P.ctl.set(P.menu(i)._2)
  
        val toggleButton = Button("Editors", (e: ReactEvent) ⇒ toggle, Button.Primary)
      }
      
      def baseProps(t: EditorDesc) = EditorBaseProps(
        userInfo    = userInfo,
        editorDesc  = t,
        cachedDataF = Cache(userInfo, t),
        ctl         = P.ctl,
        onResult    = onResult
      )

      val navElem = MuiLeftNav(
        menuItems     = nav.menuItems,
        docked        = false,
        selectedIndex = currentTable.index,
        ref           = leftNavRef.set,
        onChange      = nav.onTableChosen
      )()

      def renderLinked(reload:  Callback,
                       linked:  StrLinkedRows,
                       viaValU: U[StrValue]): ReactNode = {
        P.lookupLinked(linked.editorId) match {
          case Some(desc) ⇒
            val create =
              EditorCreateRow(desc)(EditorCreateRow.Props(
                baseProps(desc), linked, viaValU, reload
              ))
            def single =
              EditorLinkedSingleRow()(EditorLinkedSingleRow.Props(
                baseProps(desc), linked, reload, create
              ))
            def multiple =
              EditorLinkedMultipleRows(linked)(EditorLinkedMultipleRows.Props(
                baseProps(desc), linked, reload, create
              ))

            <.div(
              linked.rows.toList match {
                case Nil        ⇒ create
                case row :: Nil ⇒ single
                case rows       ⇒ multiple
              }
            )
          case None ⇒ <.div(s"Editor ${linked.editorId} is not exported")
        }
      }

    }


    def render(P: Props, S: State) = {
      val fp = fromProps.value()
      <.div(
        Styles.body,
        MuiAppBar(
          title               = s"Slick-Crud${fp.currentTable.opt.fold("")(t ⇒ s": ${t.title}")}",
          iconElementLeft     = fp.nav.toggleButton,
          showMenuIconButton  = true
        )(),
        fp.navElem,
        <.div(
          <.div(Style.container,
            P.current match {
              case RouteChooseEditor         ⇒
                <.h2("Choose an editor")

              case RouteEditor(t)        ⇒
                EditorMultipleRows(t)(EditorMultipleRows.Props(
                  fp.baseProps(t)
                ))

              case RouteEditorRow(t, rowId) ⇒
                EditorSingleRow(t, rowId)(EditorSingleRow.Props(
                  fp.baseProps(t),
                  rowId        = rowId,
                  renderLinked = fp.renderLinked
                ))

              case RouteCreateRow(t) ⇒
                EditorCreateRow(t)(EditorCreateRow.Props(
                  fp.baseProps(t),
                  wasLinkedU    = uNone,
                  linkedViaValU = uNone,
                  onCreateU     = uNone
                ))
            }
          ),
          UpdateNotifierClient(S.res)
        )
      )
    }
  }

  val component = ReactComponentB[Props]("EditorController")
    .initialState(State(Nil))
    .renderBackend[Backend]
    .configure(ComponentUpdates.inferred("EditorController"))
    .configureSpec(ThemeInstaller.installMuiContext())
    .build

  def apply(menu:        Seq[RouteEditor],
            currentPage: Route)
           (ctrl:        RouterCtl[Route]) = {
    val namedEditors = menu.map {
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
