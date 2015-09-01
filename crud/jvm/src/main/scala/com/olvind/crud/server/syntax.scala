package com.olvind.crud
package server

import slick.lifted.CanBeQueryCondition

trait syntax extends tableRefs {
  import driver.api._

  /**
   * A reference to a slick table
   *
   * @param table the TableQuery[E] of the table we want to expose
   * @param idCol a function that maps the default projection 'TABLE'
   *              to its primary key column.
   *              Multi-column primary keys are not supported.
   */
  def TableRef[ID: ColumnType: Cell, TABLE <: AbstractTable[_]]
              (table:       TableQuery[TABLE],
               isEditable:  Boolean = true,
               canDelete:   Boolean = false)
              (idCol:       TABLE ⇒ Rep[ID])
              (implicit cr: CellRow[TABLE#TableElementType]) =

    BaseTableRef[ID, TABLE](table, isEditable, canDelete, idCol, implicitly, cr)

  implicit class TableRefOps[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]){

    def projected[OLP, OP: CellRow]
                 (q: ref.Q ⇒ Query[OLP, OP, Seq]): TableRef[ID, TABLE, OLP, OP] =
      ProjectedTableRef[ID, TABLE, LP, P, OLP, OP](ref, q, implicitly)

    /**
     * Combine this editor with `other`
     *  where `fromCol` of a given row matches `other`s' `toCol`
     */
    def linkedOn[OID, OTABLE <: AbstractTable[_], OLP, OP,
                 C: Cell: FlatRepShape, OC: FlatRepShape: Cell, R]
                (fromCol: LP ⇒ Rep[C],
                 other:   ⇒ TableRef[OID, OTABLE, OLP, OP])
                (toCol:   OLP ⇒ Rep[OC])
                (pred:    (Rep[C], Rep[OC]) ⇒ Rep[R])
                (implicit ev: CanBeQueryCondition[Rep[R]]): TableRef[ID, TABLE, LP, P] =
      LinkingTableRef[ID, TABLE, LP, P, OID, OTABLE, OLP, OP, C, OC, R](ref, fromCol, toCol, pred)(other)
  }
}
