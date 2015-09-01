package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.{MuiToolbar, MuiToolbarGroup, MuiToolbarTitle}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js

object EditorToolbar {

  case class Props(
    editorDesc:        EditorDesc,
    rows:              Int,
    cachedDataOpt:     Option[CachedData],
    filterU:           U[Filter],
    openFilterDialogU: U[Callback],
    isLinkedU:         U[StrLinkedRows],
    refreshU:          U[Callback],
    showAllU:          U[Callback],
    deleteU:           U[Callback],
    showCreateU:       U[(Boolean, ReactEvent ⇒ Callback)],
    customElemU:       U[ReactElement]
  )

  private final case class Backend($: BackendScope[Props, Unit]){

    def title(P: Props) = {
      val (main: String, numRowsOpt: Option[String]) =
        (P.editorDesc.title, P.rows) match {
          case (name, 0) ⇒ (s"Create new row for $name", None)
          case (name, n) ⇒ (name, s"showing $n".some)
        }

      val suffixes = Seq[Option[String]](
        numRowsOpt,

        P.cachedDataOpt map {
          case CachedData(_, length) ⇒ s"of ${length.rows} rows"
        } filter (_ ⇒ P.rows > 0),

        P.filterU.toOption map {
          case Filter(c, v) ⇒ s"for ${c.name.value} like ${v.value}"
        },

        P.isLinkedU.toOption map {
          case StrLinkedRows(_, fromCol, toCol, _) ⇒
            s"linked on ${toCol.name.value} = ${fromCol.table.value}.${fromCol.name.value}"
        }
      )
      main + suffixes.flatten.mkString(" (", " ", ")")
    }

    private def style: js.Any =
      js.Dynamic.literal(
        justifyContent = "space-between",
        alignItems     = "center",
        display        = "flex",
        height         = "inherit"
      )

    private def styleSep: js.Any =
      js.Dynamic.literal(
        float = "inherit",
        margin = "5px"
      )

    def render(P: Props) = {
      val btns =
        List[Option[ReactNode]](
          P.showCreateU.toOption.map{
            case (isShowing, f) ⇒
              Button("Create new row", f, if (isShowing) Button.Secondary else Button.Normal)
          },
          P.deleteU.toOption.map(f ⇒
            Button("Delete row", (e: ReactEvent) ⇒ f, Button.Normal)
          ),
          P.openFilterDialogU.toOption.map(f ⇒
            Button("Filter rows", (e: ReactEvent) ⇒ f, Button.Normal)
          ),
          P.showAllU.toOption.map(f ⇒
            Button(s"See all ${P.editorDesc.mainTable.value}", (e: ReactEvent) ⇒ f, Button.Normal)
          ),
          P.refreshU.toOption.map(f ⇒
            Button("Refresh", (e: ReactEvent) ⇒ f, Button.Normal)
          ),
          P.customElemU.toOption
        )

      MuiToolbar(style = style)(
        MuiToolbarGroup(key = "left")(MuiToolbarTitle(text = title(P))()),
        MuiToolbarGroup(key = "right")(intersperse[ReactNode](btns.flatten, <.span(^.padding := "5px")) :_*)
      )
    }
  }


  private val component = ReactComponentB[Props]("EditorToolbar")
    .stateless
    .renderBackend[Backend]
    .configure(ComponentUpdates.inferredNoState("EditorToolbar"))
    .build

  def apply(p: Props): ReactElement =
    component(p)
}
