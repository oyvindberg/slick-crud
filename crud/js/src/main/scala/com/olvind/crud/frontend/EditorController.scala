package com.olvind.crud
package frontend

import autowire._
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
    menu:    Seq[(EditorName, RouteEditor)],
    current: Route,
    ctl:     RouterCtl[Route]
  )

  case class State(
    res: List[TimedT[CrudResult]]
  )

  case class Backend($: WrapBackendScope[Props, State]) {
    object cachedData {
      var cachedDataVar = Map.empty[EditorId, CFuture[CachedData]]

      val nothing = ReusableFn((in: TimedT[CrudResult]) ⇒ Callback.empty)

      def cleanup(eid: EditorId)(fail: CrudFailure) = Callback {
        cachedDataVar = cachedDataVar - eid
      }

      def fetch(t: EditorDesc): CFuture[CachedData] = {
        val remote = AjaxCall.forEditor(t.editorId)[Editor]
        val async  = AsyncCallback(nothing, cleanup(t.editorId), t.editorId)
        async("Could not load cached data", remote.cachedData(userInfo).call()).runNow().map(_.cd)
      }

      def save(eid: EditorId)(fcd: CFuture[CachedData]): Unit =
        cachedDataVar = cachedDataVar.updated(eid, fcd)

      def apply(t: EditorDesc): CFuture[CachedData]  =
        cachedDataVar getOrElse(t.editorId, fetch(t) <| save(t.editorId))

      def invalidate(): Unit =
        cachedDataVar = Map.empty
    }

    object currentTable {
      def index: U[Int] = 
        $.props.menu.indexWhere(_._2.t.some =:= opt).undef.filterNot(_ =:= -1)

      def opt: Option[EditorDesc] = $.props.current.some collect {
        case RouteEditor(t) ⇒ t
      }
    }

    val onResult: (TimedT[CrudResult]) ~=> Callback =
      ReusableFn {
        case (t: TimedT[CrudResult]) ⇒
          Callback {
            t.value match {
              case Created(_, _)| Deleted(_, _) ⇒ cachedData.invalidate()
              case _                            ⇒ ()
            }

            $.modState(s ⇒ s.copy(res = t +: s.res)).runNow()

            js.timers.setTimeout(UpdateNotifierClient.ttl){
              $.modState(s ⇒ s.copy(res = s.res.filterNot(_ =:= t))).runNow()
            }
          }
      }

    object nav {
      val leftNavRef = "leftNav"

      val menuItems: js.Array[MuiMenuItem] =
        $.props.menu.toJsArray.map{
          case (name, r) ⇒ MuiMenuItem(text = name.value, payload = r.t.mainTable.value)
        }

      val toggle: Callback =
        Callback($.$.refs(leftNavRef).foreach(_.asInstanceOf[MuiLeftNavM].toggle()))

      val onTableChosen: (ReactEvent, Int, js.Object) ⇒ Callback =
        (e, i, o) ⇒ $.props.ctl.set($.props.menu(i)._2)

      val toggleButton = Button("Editors", (e: ReactEvent) ⇒ toggle, Button.Primary)
    }

    val userInfo = UserInfo("arne")

    def baseProps(t: EditorDesc) = EditorBaseProps(
      userInfo    = userInfo,
      editorDesc       = t,
      cachedDataF = cachedData(t),
      ctl         = $.props.ctl,
      onResult    = onResult
    )

    def renderLinked(reload:  Callback,
                     linked:  StrLinkedRows,
                     viaValU: U[StrValue]): ReactNode = {
      val create =
        EditorCreateRow(linked.desc)(EditorCreateRow.Props(
          baseProps(linked.desc), linked, viaValU, reload
        ))
      def single =
        EditorLinkedSingleRow()(EditorLinkedSingleRow.Props(
          baseProps(linked.desc), linked, reload, create
        ))
      def multiple =
        EditorLinkedMultipleRows(linked)(EditorLinkedMultipleRows.Props(
          baseProps(linked.desc), linked, reload, create
        ))

      <.div(
          linked.rows.toList match {
            case Nil        ⇒ create
            case row :: Nil ⇒ single
            case rows       ⇒ multiple
          }
        )
    }

    def render(S: State) = {
      <.div(
        Styles.body,
        MuiAppBar(
          title               = s"Slick-Crud${currentTable.opt.fold("")(t ⇒ s": ${t.title}")}",
          iconElementLeft     = nav.toggleButton,
          showMenuIconButton  = true
        )(),
        MuiLeftNav(
          menuItems     = nav.menuItems,
          docked        = false,
          selectedIndex = currentTable.index,
          ref           = nav.leftNavRef,
          onChange      = nav.onTableChosen
        ),
        <.div(
          <.div(Style.container,
            $.props.current match {
              case RouteChooseEditor         ⇒
                <.h2("Choose an editor")

              case RouteEditor(t)        ⇒
                EditorMultipleRows(t)(EditorMultipleRows.Props(
                  baseProps(t)
                ))

              case RouteEditorRow(t, rowId) ⇒
                EditorSingleRow(t, rowId)(EditorSingleRow.Props(
                  baseProps(t),
                  rowId        = rowId,
                  renderLinked = renderLinked
                ))

              case RouteCreateRow(t) ⇒
                EditorCreateRow(t)(EditorCreateRow.Props(
                  baseProps(t),
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
    .backend($ ⇒ Backend(WrapBackendScope($)))
    .render($ ⇒ $.backend.render($.state))
    .configure(ComponentUpdates.inferred("EditorController"))
    .configureSpec(installMuiContext)
    .build

  def apply(menu:        Seq[RouteEditor],
            currentPage: Route)
           (ctrl:        RouterCtl[Route]) = {
    val namedEditors = menu.collect {
      case r@RouteEditor(EditorDesc(Some(name), _, _, _, _, _)) => name -> r
    }
    println(namedEditors)
    component(Props(namedEditors, currentPage, ctrl))
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
