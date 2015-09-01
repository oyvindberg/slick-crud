package com.olvind.crud
package frontend

import autowire._
import chandu0101.scalajs.react.components.materialui._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._
import upickle.default

import scalacss.ScalaCssReact._

/* Things every Editor has */
case class EditorBaseProps(
  userInfo:    UserInfo,
  table:       ClientTable,
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
    
    final override def handleUpdated(id: StrRowId): Callback =
      $.state.flatMap{ S ⇒
        asyncCb(s"Couldn't reload row $id", remote.readRow($.props.base.userInfo, id).call())
          .commit(read ⇒ patchRow(id, read.row))
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
      $.props.reload

    final override def handleDeleted(id: StrRowId): Callback =
      $.props.reload
  }
}
/**
 * An Editor that is able to update cells
 */
trait EditorBaseUpdater extends EditorBase {
  trait BackendBU[P <: PropsB, S <: StateB[S]] extends BackendB[P, S]{
    def handleUpdated(id: StrRowId): Callback
    def handleDeleted(id: StrRowId): Callback

    final def updateValue(id: StrRowId)(c: ColumnInfo)(v: StrValue): Callback =
      asyncCb(s"Could not update row $id", remote.update($.props.base.userInfo, id, c, v).call())
        .commit(_ ⇒ handleUpdated(id))

    final def deleteRow(id: StrRowId): Callback =
      asyncCb(s"Could not delete row $id", remote.delete($.props.base.userInfo, id).call())
        .commit(_ ⇒ handleDeleted(id))
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

    def renderData(S: S, t: ClientTable, data: Data): ReactElement

    def setData(data: DataState, cb: Callback): Callback =
      $.modState(_.withDataState(data), cb)

    override final def handleFailure(fail: CrudFailure): Callback =
      $.modState(_.withDataState(ErrorState(fail)))

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
    )

    override final def render(P: P, S: S): ReactElement = {
      val content: ReactElement = S.data match {
        case InitialState      ⇒ renderWaiting
        case ErrorState(fail)  ⇒ <.pre(<.code(fail.formatted))
        case HasDataState(row) ⇒ renderData(S, $.props.base.table, row)
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

    final def table = base.table
  }

  trait StateB[S <: StateB[S]] {
    def cachedDataU: U[CachedData]
    def withCachedData(cd: CachedData): S
  }

  trait BackendB[P <: PropsB, S <: StateB[S]] {
    def $: WrapBackendScope[P, S]

    def handleFailure(fail: CrudFailure): Callback = Callback.empty

    def init: Callback =
      $.props.base.cachedDataF commit {
        cd ⇒ $.modState(_.withCachedData(cd))
      }

    def reInit: Callback =
      Callback.empty

    def render(P: P, S: S): ReactElement

    final lazy val remote: ClientProxy[Editor, String, default.Reader, default.Writer] =
      AjaxCall.forTable($.props.base.table.name)[Editor]

    final lazy val asyncCb: AsyncCallback =
      AsyncCallback($.props.base.onResult, handleFailure, $.props.base.table.name)

    final lazy val showSingleRow: RouterCtl[StrRowId] =
      $.props.base.ctl.contramap[StrRowId](id ⇒ RouteEditorRow($.props.base.table, id))

    final lazy val showAllRows: Callback =
      $.props.base.ctl.set(RouteEditor($.props.table))
  }
}
