package com.olvind.crud
package frontend

import autowire._
import chandu0101.scalajs.react.components.materialui._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import upickle.default

import scalacss.ScalaCssReact._

/* Things every Editor has */
case class EditorBaseProps(
  userInfo:    UserInfo,
  editorDesc:  EditorDesc,
  onResult:    TimedT[CrudResult] ~=> Callback,
  cachedDataF: CFuture[CachedData],
  ctl:         RouterCtl[Route]
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
      $.state.flatMap{ S ⇒
        fromProps.value().asyncCb.applyEither(
          s"Couldn't reload row $id",
          remote => u => remote.readRow(u, id).call())(
          ignoreError
        ).commit(read ⇒ read.row.fold(handleNoRowFoundOnUpdate(id))(row => patchRow(id, row)))
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

    final def handleUpdateFailed(f: UpdateFailed): Callback =
      $.modState(_.withAddedValidationFails(Seq(ValidationError(f.id.some, f.col, f.e))))

    final def updateValue(id: StrRowId)(c: ColumnRef)(v: StrValue): Callback =
      fromProps.value().asyncCb.applyEither(
        s"Could not update row $id",
        remote => u => remote.update(u, id, c, v).call())(
        handleUpdateFailed
      ).commit(_ ⇒ handleUpdated(id))

    final def deleteRow(id: StrRowId): Callback =
      fromProps.value().asyncCb.applyEither(
        s"Could not delete row $id",
        remote => u => remote.delete(u, id).call())(
        criticalError
      ).commit(_ ⇒ handleDeleted(id))
  }
}

/**
 * An Editor which is responsible to fetching data from remote
 */
trait EditorBasePrimary extends EditorBase {

  sealed trait DataState{def opt: Option[Data] = None}
  case object InitialState                  extends DataState
  case class  HasDataState(data: Data)      extends DataState{override def opt = data.some}
  case class  ErrorState(fail: CrudFailure) extends DataState
  
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

    final lazy val renderWaiting = MuiCircularProgress(
      size = 2.0,
      mode = MuiProgressMode.INDETERMINATE
    )()

    override final def render(P: P, S: S): ReactElement = {
      val content: ReactElement = S.data match {
        case InitialState      ⇒ renderWaiting
        case ErrorState(fail)  ⇒ <.pre(<.code(fail.formatted))
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

    final def editorDesc = base.editorDesc
  }

  trait StateB[S <: StateB[S]] {
    def cachedDataU: U[CachedData]
    def validationFails: Seq[ValidationError]

    def withCachedData(cd: CachedData): S
    def withValidationFails(ves: Seq[ValidationError]): S

    final def withAddedValidationFails(ves: Seq[ValidationError]) =
      withValidationFails(validationFails ++ ves)

    final def withNoValidationFails =
      withValidationFails(Seq.empty)
  }

  trait BackendB[P <: PropsB, S <: StateB[S]] {
    def $: BackendScope[P, S]

    implicit def r: Reusability[P]

    final def ps: CallbackTo[(P, S)] = $.props.flatMap(p => $.state.map(s => (p, s)))

    def init: Callback =
      $.props.flatMap(_.base.cachedDataF commit {
        cd ⇒ $.modState(_.withCachedData(cd))
      })

    def reInit: Callback =
      Callback.empty

    def render(P: P, S: S): ReactElement

    def clearValidationFail(idOpt: Option[StrRowId])(c: ColumnRef): Callback =
      $.modState(s => s.withValidationFails(
        s.validationFails.filterNot(ve => ve.c =:= c && ve.rowIdOpt =:= idOpt)
      ))

    final def ignoreError(fail: CrudFailure): Callback =
      Callback.empty

    final def criticalError(fail: CrudFailure): Callback =
      Callback(dom.alert(fail.formatted))

    lazy val fromProps = Px.cbA($.props).map(FromProps)

    final case class FromProps(P: P){
      val remote: ClientProxy[Editor, String, default.Reader, default.Writer] =
        AjaxCall.forEditor(P.base.editorDesc.editorId)[Editor]

      val asyncCb: AsyncCallback =
        AsyncCallback(remote, P.base.userInfo, P.base.onResult, criticalError, P.base.editorDesc.editorId)

      val showSingleRow: RouterCtl[StrRowId] =
        P.base.ctl.contramap[StrRowId](id ⇒ RouteEditorRow(P.base.editorDesc, id))

      val showAllRows: Callback =
        P.base.ctl.set(RouteEditor(P.editorDesc))
    }
  }
}
