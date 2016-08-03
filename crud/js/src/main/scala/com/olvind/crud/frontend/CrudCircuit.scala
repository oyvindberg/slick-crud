package com.olvind.crud
package frontend

import autowire._
import diode.ActionResult._
import diode._
import diode.react.ReactConnector
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.ReusableFn

import scala.concurrent.Future

final case class CrudCircuit(initialModel: Model) extends Circuit[Model] with ReactConnector[Model] {
  val sendCb: ReusableFn[Action, Callback] =
    ReusableFn(a ⇒ Callback(dispatch(a)))

  private def remote(eid: EditorId) =
    AjaxCall(eid.value)[Editor]

  def effectOf[T](fromAction: Action, eid: EditorId, xResF: ⇒ Future[XRes[T]])(f: (Model, T) ⇒ Model): Effect =
    Effect(
      xResF.map(
        (xRes: XRes[T]) ⇒
          ChangeModel(
            (model: Model) ⇒
              xRes match {
                case XSuccess(t) ⇒
                  f(model, t)
                case XTechnicalMsg(msg) ⇒
                  model.copy(errorOpt = Some((msg, fromAction)))
                case XUserMsg(msg) ⇒
                  model.copy(notifications = model.notifications :+ msg)
                case XValidation(idOpt, ves: Seq[(ColumnRef, ValidationError)]) =>
                  ???
//                  model.updatedEditor(eid)(em ⇒ em.copy(validationErrors = em.validationErrors.updated(model.currentRowIdOpt, vs.toMap)))
              }
          )
      )
    )

  def readEffect(fromAction: Action, userInfo: UserInfo, eid: EditorId, queryParams: QueryParams, append: Boolean): Effect =
    effectOf(fromAction, eid, remote(eid).read(userInfo, Some(queryParams)).call()){
      case (model: Model, rows: Seq[StrTableRow]) ⇒
        model.updatedEditor(eid)(
          em ⇒ em.copy(rows = Some(if (append) em.rows.getOrElse(Seq.empty[StrTableRow]) ++ rows else rows))
        )
    }

  def realActionHandler(currentModel: Model, userInfo: UserInfo, data: Map[EditorId, EditorModel]): Action => ActionResult[Model] = {
    case a@FetchEditorData(eid) ⇒
      val readEditor: Effect =
        readEffect(a, userInfo, eid, data(eid).queryParams, append = false)
      val readCachedData: Effect =
        Effect.action(FetchCachedData(eid))

      EffectOnly(readEditor >> readCachedData)

    case rfa: ReadFurtherAction ⇒
      val (newQueryParams: QueryParams, append: Boolean) =
        rfa match {
          case FetchFilteredData(eid, filterOpt) ⇒
            (data(eid).queryParams.copy(filterOpt = filterOpt), false)
          case FetchSortedData(eid, column) ⇒
            (data(eid).queryParams.withSortedBy(column), false)
          case FetchMoreData(eid) ⇒
            (data(eid).queryParams.copy(page = data(eid).queryParams.page.next), true)
        }

      val effect = readEffect(rfa, userInfo, rfa.eid, newQueryParams, append)

      ModelUpdateEffect(currentModel.updatedEditor(rfa.eid)(em ⇒ em.copy(queryParams = newQueryParams)), effect)

    case FetchCachedData(eid) ⇒
      currentModel.cachedData.get(eid) match {
        case Some(exists) ⇒
          NoChange
        case None ⇒
          val effect = Effect(remote(eid).cachedData(userInfo).call().map(
            cd ⇒ ChangeModel(model ⇒ model.copy(cachedData = model.cachedData.updated(eid, cd)))
          ))
          EffectOnly(effect)
      }

    case a@FetchRow(eid, rowId) ⇒
      val readRow: Effect =
        effectOf(a, eid, remote(eid).readRow(userInfo, rowId).call()){
          case (model, None) ⇒
            model.copy(errorOpt = Some((s"Row $rowId not found", a)))

          case (model, Some(readRow: StrTableRow)) ⇒
            model.updatedEditor(eid){
              (em: EditorModel) ⇒
                val patchedRows: Seq[StrTableRow] =
                  em.rows match {
                    case Some(rows) ⇒
                      rows map {
                        case StrTableRow(Some(`rowId`), _) ⇒ readRow
                        case other                         ⇒ other
                      }
                    case None ⇒ Seq(readRow)
                  }
                em.copy(rows = Some(patchedRows))
            }
        }
      val readLinked =
        Effect.action(FetchLinkedRows(eid, rowId))
      val readCached =
        Effect.action(FetchCachedData(eid))

      EffectOnly(readRow >> readLinked >> readCached)

    case a@FetchLinkedRows(eid, rowId) ⇒
      currentModel.editor(eid).flatMap(_.linkedRows.get(rowId)) match {
        case Some(exists) ⇒
          NoChange
        case None ⇒
          EffectOnly(
            effectOf(a, eid, remote(eid).readLinkedRows(userInfo, rowId).call()) {
              case (model, linkedRows: Seq[StrLinkedRows]) ⇒
                model.updatedEditor(eid) {
                  em ⇒ em.copy(linkedRows = em.linkedRows.updated(rowId, linkedRows))
                }
            }
          )
      }

    case RefreshEditor(eid) ⇒
      val newModel: Model =
        currentModel.updatedEditor(eid) {
          em ⇒ EditorModel(em.editor, None, Map.empty, em.queryParams, Map.empty)
        }
      val effect: Effect =
        Effect.action(FetchEditorData(eid))

      ModelUpdateEffect(newModel, effect)

    case a@UpdateValue(eid, rowId, col, value) ⇒
      val updateValue: Effect =
        effectOf(a, eid, remote(eid).update(userInfo, rowId, col, value).call()){
          case (model, (oldValue, newValue)) ⇒
            model.copy(notifications = model.notifications :+ s"Changed $oldValue to $newValue")
        }
      val readRow: Effect =
        Effect(Future(FetchRow(eid, rowId)))

      EffectOnly(updateValue >> readRow)

    case a@CreateRow(eid, params) ⇒
      val created: Effect =
        effectOf(a, eid, remote(eid).create(userInfo, params).call()){
          case (model: Model, newRowOpt) ⇒
            dispatch(Navigate(newRowOpt.fold[Route](RouteEditor(eid))(row ⇒ RouteEditorRow(eid, row))))
            model.copy(notifications = model.notifications :+ s"Created row with id $newRowOpt")
        }
      EffectOnly(created)

    case ClearValidationError(eid, rowId, colOpt) ⇒
      val newModel = currentModel.updatedEditor(eid) { em =>
        val updated = em.validationErrors.get(rowId) match {
          case Some(map) ⇒
            colOpt match {
              case Some(col) ⇒
                em.validationErrors.updated(rowId, map - col)
              case None ⇒
                em.validationErrors - rowId
            }
          case None ⇒ em.validationErrors
        }
        em.copy(validationErrors = updated)
      }
      ModelUpdate(newModel)

    case a@DeleteRow(eid, rowId) ⇒
      val effect = effectOf(a, eid, remote(eid).delete(userInfo, rowId).call()){
        case (model, ()) ⇒
          dispatch(Navigate(RouteEditor(eid)))
          currentModel.updatedEditor(eid) { em =>
            val newRows = em.rows map {
              rows ⇒ rows.filterNot(row ⇒ row.idOpt.contains(rowId))
            }
            em.copy(rows = newRows)
          }
      }

      EffectOnly(effect)

    case Navigate(route) ⇒
      currentModel.routerCtl.foreach(_.set(route).runNow())
      NoChange
  }

  override protected def actionHandler: HandlerFunction = {
    case a@(model, ChangeModel(f)) ⇒
      Some(ModelUpdate(f(model)))
    case a@(model, SetRouterCtl(routerCtl)) ⇒
      Some(ModelUpdate(model.copy(routerCtl = Some(routerCtl))))
    case (model@Model(_, userInfo, _, _, _, data), action: Action) ⇒
      println(action)
      Some(realActionHandler(model, userInfo, data)(action))
  }
}