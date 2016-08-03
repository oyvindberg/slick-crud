package com.olvind.crud
package frontend

import com.olvind.stringifiers.DecodeFail
import diode.react.ModelProxy
import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.extra.Reusability

trait ReusabilityInstances {

  /**
    * Values that goes across the wire we check by contents
    */
  implicit val RColumnRef:  Reusability[ColumnRef]  = Reusability.by_==
  implicit val RDecodeFail: Reusability[DecodeFail] = Reusability.by_==
  implicit val RFilter:     Reusability[Filter]     = Reusability.by_==
  implicit val RSortOrder:  Reusability[SortOrder]  = Reusability.by_==
  implicit val REditorDesc: Reusability[EditorDesc] = Reusability.by_==
  implicit val RColumnDesc: Reusability[ColumnDesc] = Reusability.by_==
  implicit val RRoute:      Reusability[Route]      = Reusability.by_==
  implicit val REditorId:   Reusability[EditorId]   = Reusability.by((x: EditorId) => x.value)
  implicit val RStrValue:   Reusability[StrValue]   = Reusability.by((x: StrValue) => x.value)
  implicit val RStrRowId:   Reusability[StrRowId]   = Reusability.by((x: StrRowId) => x.value)
  implicit val RTableName:  Reusability[TableName]  = Reusability.by((x: TableName) => x.value)

  /**
    * Expensive values that we get over the wire by demand we check
    *  by reference only
    */
  implicit val RCachedData:    Reusability[CachedData]    = Reusability.byRef
  implicit val RStrLinkedRows: Reusability[StrLinkedRows] = Reusability.byRef
  implicit val RStrTableRow:   Reusability[StrTableRow]   = Reusability.byRef
  implicit def RMap[K, V]:     Reusability[Map[K, V]]     = Reusability.byRef

  /**
    *  As a rule, we never encode ids etc *only* in callbacks, so
    *   we ignore them when considering if we should re-render.
    *
    *  This is not in general a good idea.
    */
  implicit def RFunction1[T1, R]:     Reusability[T1 => R]       = Reusability.always
  implicit def RFunction2[T1, T2, R]: Reusability[(T1, T2) => R] = Reusability.always
  implicit def RCallback[A]:          Reusability[CallbackTo[A]] = Reusability.always

  /* Gracefully handle if we should get a new modelProxy */
  implicit def RModelProxy[T]:        Reusability[ModelProxy[T]] = Reusability.byRef
}
