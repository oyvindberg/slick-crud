package com.olvind.crud
package server

import slick.ast.BaseTypedType

trait slickHacks extends slickIntegration {
  import driver.api._

  object slickHacks {
    /* the default implicit, queryDeleteActionExtensionMethods, only converts Query[_ <: Table[_]]) to DeleteActionExtensionMethods,
     *  so I changed it here to make it work for Query[_ <: AbstractTable[_]] too */
    implicit def queryDeleteActionExtensionMethods[C[_]](q: Query[_ <: AbstractTable[_], _, C]): driver.DeleteActionExtensionMethods =
      driver.createDeleteActionExtensionMethods(driver.deleteCompiler.run(q.toNode).tree, ())

    /* enable Rep[Any] - we have to guarantee on the outside that what we do is sane */
    implicit val anyEvidence: BaseTypedType[Any] = null

    /* this is needed to hack around a case where a column is declared as column[T], but used in
     *   the table projection as a column[Option[T]]
     */
    def ensureOptionalColumn(value: Any)(c: Rep[Any]) =
      (value, c.toNode) match {
        case (v: Option[_], slick.ast.OptionApply(_)) ⇒ c
        case (v: Option[_], _                       ) ⇒ c.?.asInstanceOf[Rep[Any]]
        case _                                        ⇒ c
      }
  }
}
