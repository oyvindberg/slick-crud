package com.olvind.crud
package server

import slick.dbio.DBIO

trait dbOps extends executionContexts {

  case class CrudDbOp[+T](res: DBIO[XRes[T]]){
    def map[U](f: T => U): CrudDbOp[U] =
      CrudDbOp(res map (_ map f))

    def mapIO[U](f: DBIO[XRes[T]] => DBIO[XRes[U]]): CrudDbOp[U] =
      CrudDbOp[U](f(res))

    def flatMap[U](f: T => CrudDbOp[U]): CrudDbOp[U] =
      CrudDbOp[U](
        res.flatMap {
          case XSuccess(t) => f(t).res
          case other       => DBIO successful other.asInstanceOf[XRes[U]]
        }
      )
  }

  object CrudDbOp {
    def apply[T](t: XRes[T]): CrudDbOp[T] =
      CrudDbOp[T](DBIO successful t)

    def success[T](t: T): CrudDbOp[T] =
      CrudDbOp[T](DBIO successful XSuccess(t))

    def failure[F <: XFail](f: F): CrudDbOp[Nothing] =
      CrudDbOp[Nothing](DBIO successful f)

    def fromOpt[F <: XFail, T](ot: Option[T], left: => F): CrudDbOp[T] =
      CrudDbOp[T](DBIO successful ot.fold[XRes[T]](left)(XSuccess.apply))

    def fromEither[F <: XFail, T](et: Either[F, T]): CrudDbOp[T] =
      CrudDbOp[T](DBIO successful et.fold(identity, XSuccess.apply))

    def fromDbio[T](iot: DBIO[T]): CrudDbOp[T] =
      CrudDbOp[T](iot map XSuccess.apply)

    def require[F <: XFail](condition: Boolean, left: => F): CrudDbOp[Unit] =
      CrudDbOp(DBIO successful (if (condition) XSuccess(()) else left))
  }
}
