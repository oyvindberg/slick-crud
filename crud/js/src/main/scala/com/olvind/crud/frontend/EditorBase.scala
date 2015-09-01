package com.olvind.crud
package frontend

import autowire._
import chandu0101.scalajs.react.components.materialui.{DeterminateIndeterminate, MuiCircularProgress}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._
import upickle.default

import scala.concurrent.Future
import scalacss.ScalaCssReact._

/* Things every Editor has */
case class EditorBaseProps(
  userInfo:      UserInfo,
  editorDesc:    EditorDesc,
  onResult:      TimedRes ~=> Callback,
  cachedDataOpt: Option[CachedData],
  ctl:           RouterCtl[Route]
)

/**
 * An Editor that shows multiple rows
 */
trait EditorBaseMultipleRows extends EditorBase {
  override final type Data = Seq[StrTableRow]
}

/**
 * An editor that shows one single row
 */
trait EditorBaseSingleRow extends EditorBase {
  override final type Data = StrTableRow
}

/**
 * An Editor that is able to update cells, and is maintains
 *  state of the data it has fetched
 */
trait EditorBaseUpdaterPrimary extends EditorBaseUpdater with EditorBasePrimary {
  trait BackendBUP[P <: PropsB, S <: StateBP[S]] extends BackendBU[P, S] with BackendBP[P, S]{
    def patchRow(id: StrRowId, row: StrTableRow): Callback

    def handleNoRowFoundOnUpdate(id: StrRowId): Callback

    final override def handleUpdated(id: StrRowId): Callback =
      async((user, remote) => remote.readRow(user, id).call(), None){
        case XSuccess(rowOpt) => rowOpt.fold(handleNoRowFoundOnUpdate(id))(row => patchRow(id, row))
      }
  }
}

/**
 * An Editor that is able to update cells, but is
 *  not responsible for maintaining it's own state
 */
trait EditorBaseUpdaterLinked extends EditorBaseUpdater {
  trait PropsBUL extends PropsB {
    def reload: Callback
  }

  trait BackendBUL[P <: PropsBUL, S <: StateB[S]] extends BackendBU[P, S]{
    final override def handleUpdated(id: StrRowId): Callback =
      $.props.flatMap(_.reload)

    final override def handleDeleted(id: StrRowId): Callback =
      $.props.flatMap(_.reload)
  }
}

/**
 * An Editor that is able to update cells
 */
trait EditorBaseUpdater extends EditorBase {
  trait BackendBU[P <: PropsB, S <: StateB[S]] extends BackendB[P, S]{
    def handleUpdated(id: StrRowId): Callback
    def handleDeleted(id: StrRowId): Callback

    final def handleUpdateFailed(row: StrRowId, vs: Seq[ValidationError]): Callback =
      $.modState(_.withAddedValidationFails(Some(row), vs))

    final def updateValue(id: StrRowId)(c: ColumnRef)(v: StrValue): Callback =
      async((user, remote) => remote.update(user, id, c, v).call(), s"Update row $id to ${v.value}".some){
        case XValidation(_, vs) => handleUpdateFailed(id, vs)
        case XSuccess(ok)       => handleUpdated(id)
      }

    final def deleteRow(id: StrRowId): Callback =
      async((user, remote) => remote.delete(user, id).call(), s"Delete row $id".some){
        case XSuccess(()) => handleDeleted(id)
      }
  }
}

/**
 * An Editor which is responsible to fetching data from remote
 */
trait EditorBasePrimary extends EditorBase {

  sealed trait DataState{def opt: Option[Data] = None}
  case object InitialState              extends DataState
  case class  HasDataState(data: Data)  extends DataState{override def opt = data.some}
  case class  ErrorState(msg: XUserMsg) extends DataState

  trait StateBP[S <: StateBP[S]] extends StateB[S]{
    def data: DataState
    def withDataState(data: DataState): S
  }

  trait BackendBP[P <: PropsB, S <: StateBP[S]] extends BackendB[P, S]{
    def loadInitialData: Callback

    def renderData(P: P, S: S, t: EditorDesc, data: Data): ReactElement

    def setData(data: DataState, cb: Callback): Callback =
      $.modState(_.withDataState(data), cb)

    override final abstract def reInit: Callback =
      $.modState(_.withDataState(InitialState)) >> loadInitialData

    override final abstract def init: Callback = {
      super.init >> $.state.map(_.data).flatMap{
        case InitialState ⇒ loadInitialData
        case _            ⇒ Callback.empty
      }
    }

    final lazy val renderWaiting: ReactElement =
      MuiCircularProgress(
        size = 2.0,
        mode = DeterminateIndeterminate.indeterminate
      )()

    override final def render(P: P, S: S): ReactElement = {
      val content: ReactElement = S.data match {
        case InitialState      ⇒ renderWaiting
        case ErrorState(fail)  ⇒ <.pre(<.code(fail.value))
        case HasDataState(row) ⇒ renderData(P, S, P.base.editorDesc, row)
      }
      <.div(
        TableStyle.centered,
        content
      )
    }
  }
}

trait EditorBase {
  type Data

  trait PropsB {
    def base: EditorBaseProps

    final def editorDesc: EditorDesc =
      base.editorDesc
  }

  trait StateB[S <: StateB[S]] {
    def cachedDataOpt: Option[CachedData]
    def validationFails: Map[Option[StrRowId], Seq[ValidationError]]

    def withCachedData(cd: CachedData): S
    def withValidationFails(rowOpt: Option[StrRowId], ves: Seq[ValidationError]): S

    final def withAddedValidationFails(rowOpt: Option[StrRowId], ves: Seq[ValidationError]): S =
      withValidationFails(rowOpt, validationFails.get(rowOpt).fold(ves)(_ ++ ves))
  }

  trait BackendB[P <: PropsB, S <: StateB[S]] {
    def $: BackendScope[P, S]

    implicit def r: Reusability[P]

    def init: Callback =
      $.props.map(p => p.base.cachedDataOpt) flatMap {
        case Some(cd) => $.modState(_.withCachedData(cd))
        case None     => Callback.empty
      }

    def reInit: Callback =
      Callback.empty

    def render(P: P, S: S): ReactElement

    def clearValidationFail(idOpt: Option[StrRowId])(c: ColumnRef): Callback =
      $.modState(s => s.withValidationFails(idOpt, Seq.empty))

    lazy val fromProps: Px[FromProps] =
      Px.cbA($.props).map(FromProps)

    sealed case class FromProps(P: P){
      val asyncInfo: AsyncCallback =
        AsyncCallback(
          P.base.userInfo,
          P.base.onResult,
          P.base.editorDesc.editorId
        )

      val showSingleRow: RouterCtl[StrRowId] =
        P.base.ctl.contramap[StrRowId](
          id ⇒ RouteEditorRow(P.base.editorDesc, id)
        )

      val showAllRows: Callback =
        P.base.ctl.set(RouteEditor(P.editorDesc))
    }

    def async[R](f: (UserInfo, ClientProxy[Editor, String, default.Reader, default.Writer]) => Future[XRes[R]],
                 logOpt: Option[String])
                (handle: PartialFunction[XRes[R], Callback]): Callback = {

      val fp     = fromProps.value
      val remote = AjaxCall.forEditor(fp.asyncInfo.editorId)[Editor]
      def coord  = Coordinate(fp.asyncInfo.editorId, rowOpt = None, colOpt = None)

      Callback.future(
        Clock{c =>
          val ff: Future[XRes[R]] =
            f(fp.asyncInfo.user, remote) recover {
              case th => XTechnicalMsg(th)
            }

          ff.map[Callback]{
            res ⇒
              fp.asyncInfo.onResult(c.timed((coord, res, logOpt))) >>
              (handle.lift(res) getOrElse Callback.empty)
          }
        }
      )
   }
  }
}
