package com.olvind.crud
package frontend

import chandu0101.scalajs.react.components.materialui.{MuiToolbar, MuiToolbarGroup, MuiToolbarTitle}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js

object EditorToolbar {

  final case class Props(
    editorDesc:          EditorDesc,
    rows:                Int,
    refresh:             Callback,
    showAll:             Callback,
    cachedDataOpt:       Option[CachedData]    = None,
    filterOpt:           Option[Filter]        = None,
    openFilterDialogOpt: Option[Callback]      = None,
    isLinkedOpt:         Option[StrLinkedRows] = None,
    deleteOpt:           Option[Callback]      = None,
    showCreateOpt:       Option[Callback]      = None,
    customElemOpt:       Option[ReactElement]  = None
  )

  private implicit val ReusableProps: Reusability[Props] =
    Reusability.by((P: Props) => (P.editorDesc, P.rows, P.cachedDataOpt))

  private final case class Backend($: BackendScope[Props, Unit]){

    def title(P: Props): String = {
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

        P.filterOpt map {
          case Filter(c, v) ⇒ s"for ${c.name.value} like ${v.value}"
        },

        P.isLinkedOpt map {
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

    def render(P: Props): ReactElement = {
      val btns =
        List[Option[ReactNode]](
          P.showCreateOpt.map(f ⇒
            Button("Create new row", Some((e: ReactEvent) ⇒ f), Button.Normal)
          ),
          P.deleteOpt.map(f ⇒
            Button("Delete row", Some((e: ReactEvent) ⇒ f), Button.Normal)
          ),
          P.openFilterDialogOpt.map(f ⇒
            Button("Filter rows", Some((e: ReactEvent) ⇒ f), Button.Normal)
          ),
          Some(
            Button(s"See all ${P.editorDesc.mainTable.value}", Some((e: ReactEvent) ⇒ P.showAll), Button.Normal)
          ),
          Some(
            Button("Refresh", Some((e: ReactEvent) ⇒ P.refresh), Button.Normal)
          ),
          P.customElemOpt
        )

      MuiToolbar(style = style)(
        MuiToolbarGroup(key = "left")(MuiToolbarTitle(text = title(P))()),
        MuiToolbarGroup(key = "right")(intersperse[ReactNode](btns.flatten, <.span(^.padding := "5px")) :_*)
      )
    }
  }


  private val component =
    ReactComponentB[Props]("EditorToolbar")
      .stateless
      .renderBackend[Backend]
      .configure(ShouldUpdate.apply)
      .build

  def apply(p: Props): ReactElement =
    component(p)
}
