package com.olvind.crud
package frontend

import autowire._
import chandu0101.scalajs.react.components.materialui._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom

import scala.scalajs.js
import scalacss.Defaults._
import scalacss.ScalaCssReact._

object EditorController {

  case class Props(
    menu:    List[RouteEditor],
    current: Route,
    ctl:     RouterCtl[Route]
  )

  case class State(
    res: List[TimedT[CrudResult]]
  )

  case class Backend($: WrapBackendScope[Props, State]) {
    object cachedData {
      var cachedDataVar = Map.empty[TableName, CFuture[CachedData]]

      val nothing = ReusableFn((in: TimedT[CrudResult]) ⇒ Callback.empty)

      def cleanup(t: TableName)(fail: CrudFailure) = Callback {
        cachedDataVar = cachedDataVar - t
      }

      def fetch(t: TableName): CFuture[CachedData] = {
        val remote = AjaxCall.forTable(t)[Editor]
        val async  = AsyncCallback(nothing, cleanup(t), t)
        async("Could not load cached data", remote.cachedData(userInfo).call()).map(_.map(_.cd)).runNow()
      }

      def save(t: TableName)(fcd: CFuture[CachedData]): Unit =
        cachedDataVar = cachedDataVar.updated(t, fcd)

      def apply(t: TableName): CFuture[CachedData]  =
        cachedDataVar getOrElse(t, fetch(t) <| save(t))

      def invalidate(): Unit =
        cachedDataVar = Map.empty
    }

    object currentTable {
      def index: U[Int] = 
        $.props.menu.indexWhere(_.t.some =:= opt).undef.filterNot(_ =:= -1)

      def opt: Option[ClientTable] = $.props.current.some collect {
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
        $.props.menu.toJsArray.map(
          p ⇒ MuiMenuItem(text = p.t.name.value, payload = p.t.name.value)
        )

      val toggle: Callback =
        Callback($.$.refs(leftNavRef).foreach(_.asInstanceOf[MuiLeftNavM].toggle()))

      val onTableChosen: (ReactEvent, Int, js.Object) ⇒ Callback =
        (e, i, o) ⇒ $.props.ctl.set($.props.menu(i))

      val toggleButton = Button("Tables", (e: ReactEvent) ⇒ toggle, Button.Primary)
    }

    val userInfo = UserInfo("arne")

    def baseProps(t: ClientTable) = EditorBaseProps(
      userInfo    = userInfo,
      table       = t,
      cachedDataF = cachedData(t.name),
      ctl         = $.props.ctl,
      onResult    = onResult
    )

    def renderLinked(reload:  Callback,
                     linked:  StrLinkedRows,
                     viaValU: U[StrValue]): ReactNode = {

      val foundTableOpt = $.props.menu.collectFirst {
        case RouteEditor(ct@ClientTable(linked.toCol.table, _, _, _)) ⇒ ct
      }

      <.div(
        foundTableOpt match {
          case Some(t) ⇒
            val create =
              EditorCreateRow(t)(EditorCreateRow.Props(
                baseProps(t), linked, viaValU, reload
              ))
            def single =
              EditorLinkedSingleRow()(EditorLinkedSingleRow.Props(
                baseProps(t), linked, reload, create
              ))
            def multiple =
              EditorLinkedMultipleRows(linked)(EditorLinkedMultipleRows.Props(
                baseProps(t), linked, reload, create
              ))

            linked.rows.toList match {
              case Nil        ⇒ create
              case row :: Nil ⇒ single
              case rows       ⇒ multiple
            }
          case None ⇒
            <.h2(s"Table ${linked.toCol.table} is not exported")
        }
      )

    }

    def render(S: State) = {
      <.div(
        Styles.body,
        MuiAppBar(
          title               = s"Slick-Crud${currentTable.opt.fold("")(t ⇒ s": ${t.name.value }")}",
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
                <.h2("Choose a table")

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

  def apply(menu:        List[RouteEditor],
            currentPage: Route)
           (ctrl:        RouterCtl[Route]) =
    component(Props(menu, currentPage, ctrl))

  object Style extends StyleSheet.Inline {
    import dsl._
    
    val container = style(
      padding(20.px),
      borderLeft(c"rgb(223, 220, 220)", 1.px, solid),
      flex := "1"
    )
  }
}
