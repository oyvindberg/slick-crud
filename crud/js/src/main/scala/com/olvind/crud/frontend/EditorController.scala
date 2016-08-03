package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.RefHolder
import chandu0101.scalajs.react.components.materialui.{MuiAppBar, MuiDrawer, MuiDrawerM, MuiMenuItem, MuiMuiThemeProvider}
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js
import scalacss.Defaults._
import scalacss.ScalaCssReact._

object EditorController {

  final case class Props(
    modelProxy: ModelProxy[Model],
    dispatch:   ReusableFn[Action, Callback],
    routerCtl:  RouterCtl[Route],
    route:      Route){
    def model: Model = modelProxy.value
  }

  final case class State(menuOpen: Boolean)

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.by((P: Props) => (P.modelProxy, P.model, P.route))

  private implicit val ReusableState: Reusability[State] =
    Reusability.caseClass[State]

  private final case class Backend($: BackendScope[Props, State]) extends TimerSupport {

    val leftNavRef: RefHolder[MuiDrawerM] =
      RefHolder[MuiDrawerM]

    val toggle: Callback =
      $.modState(s => s.copy(menuOpen = !s.menuOpen))

    def menuItems(model: Model, dispatch: ReusableFn[Action, Callback]): js.Array[ReactElement] =
      model.data.map(_._2.editor).toJsArray.map{
        editor ⇒
          MuiMenuItem[EditorDesc](
            key         = editor.editorId.value,
            primaryText = editor.editorName.value,
            value       = editor,
            onTouchTap  = (e: ReactTouchEventH) => dispatch(Navigate(RouteEditor(editor.editorId))) >> toggle
          )()
      }

    def renderLinked(model:     Model,
                     dispatch:  ReusableFn[Action, Callback],
                     reload:    Callback)
                    (linked:    StrLinkedRows,
                     prefilled: Map[ColumnRef, StrValue]): ReactNode =

      model.editor(linked.editorId) match {
        case Some(editorModel) ⇒
          val create =
            EditorCreateRow(EditorCreateRow.Props(
              sendAction      = dispatch,
              model           = editorModel,
              cachedDataOpt   = model.cachedData.get(editorModel.editor.editorId),
              wasLinkedOpt    = Some(linked),
              prefilled       = prefilled
            ))

          <.div(
            linked.rows.toList match {
              case Nil ⇒
                create

              case row :: Nil ⇒
                EditorLinkedSingleRow(
                  EditorLinkedSingleRow.Props(
                    sendAction    = dispatch,
                    editor        = editorModel.editor,
                    cachedDataOpt = model.cachedData.get(editorModel.editor.editorId),
                    linkedRow     = linked,
                    reload        = reload,
                    createElem    = create
                  )
                )

              case rows ⇒
                EditorLinkedMultipleRows(
                  EditorLinkedMultipleRows.Props(
                    sendAction    = dispatch,
                    editor        = editorModel.editor,
                    cachedDataOpt = model.cachedData.get(editorModel.editor.editorId),
                    linkedRows    = linked,
                    reload        = reload,
                    createElem    = create
                  )
                )
            }
          )
        case None ⇒
          <.div(s"Editor ${linked.editorId} is not exported")
      }

    def render(P: Props, S: State): ReactElement = {
      object Editor {
        def unapply(eid: EditorId): Option[EditorModel] =
          P.model.editor(eid)
      }

      MuiMuiThemeProvider()(
        <.div(
          Styles.body,
          MuiAppBar(
            title              = s"Slick-Crud${P.model.currentEditorIdOpt(P.route).flatMap(P.model.editor).fold("")(e ⇒ s": ${e.editor.title}")}",
            iconElementLeft    = Button("Editors", Some(_ ⇒ toggle), Button.Primary),
            showMenuIconButton = true
          )(),
          MuiDrawer(
            docked          = false,
            ref             = leftNavRef.set,
            open            = S.menuOpen,
            onRequestChange = (open: Boolean, reason: String) => $.modState(_.copy(menuOpen = open))
          )(menuItems(P.model, P.dispatch)),
          <.div(
            <.div(Style.container,
              P.route match {
                case RouteEditor(Editor(editorModel)) ⇒
                  EditorMultipleRows(EditorMultipleRows.Props(
                    sendAction    = P.dispatch,
                    model         = editorModel,
                    cachedDataOpt = P.model.cachedData.get(editorModel.editor.editorId)
                  ))

                case RouteEditorRow(Editor(editorModel), rowId) ⇒
                  EditorSingleRow(EditorSingleRow.Props(
                    sendAction    = P.dispatch,
                    model         = editorModel,
                    cachedDataOpt = P.model.cachedData.get(editorModel.editor.editorId),
                    rowId         = rowId,
                    renderLinked  = renderLinked(
                      model    = P.model,
                      dispatch = P.dispatch,
                      reload   = P.dispatch(FetchLinkedRows(editorModel.editor.editorId, rowId))
                    )
                  ))

                case RouteCreateRow(Editor(editorModel)) ⇒
                  EditorCreateRow(EditorCreateRow.Props(
                    sendAction      = P.dispatch,
                    model           = editorModel,
                    cachedDataOpt   = P.model.cachedData.get(editorModel.editor.editorId),
                    wasLinkedOpt    = None,
                    prefilled       = Map.empty
                  ))

                case _ ⇒
                  <.h2("Choose an editor")
              }
            )
          )
        )
      )
    }
  }

  private val component =
    ReactComponentB[Props]("EditorController")
      .initialState_P(P ⇒ State(P.route =:= RouteChooseEditor))
      .renderBackend[Backend]
      .configure(ShouldUpdate.apply)
      .componentWillMount($ ⇒ $.props.modelProxy.dispatch(SetRouterCtl($.props.routerCtl)))
      .build

  def apply(modelProxy: ModelProxy[Model],
            dispatcher: ReusableFn[Action, Callback],
            route:      Route)
           (ctl:        RouterCtl[Route]): ReactElement =
    component(Props(modelProxy, dispatcher, ctl, route))

  object Style extends StyleSheet.Inline {
    import dsl._
    
    val container = style(
      padding(20.px),
      borderLeft(c"rgb(223, 220, 220)", 1.px, solid),
      flex := "1"
    )
  }
}
