package com.olvind.crud
package server

import com.typesafe.scalalogging.LazyLogging

trait UpdateNotifierLogging extends UpdateNotifier {
  self: LazyLogging ⇒

  override abstract def notifySuccess(user: UserInfo)(s: CrudSuccess) = {
    super.notifySuccess(user)(s)
    s match {
      case _: Read ⇒ ()
      case _       ⇒ logger.info(s.formatted)
    }
  }

  override abstract def notifyFailure(user: UserInfo)(f: CrudFailure) = {
    super.notifyFailure(user)(f)
    logger.warn(f.formatted)
  }
}
