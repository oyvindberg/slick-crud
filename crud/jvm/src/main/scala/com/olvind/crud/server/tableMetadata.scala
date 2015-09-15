package com.olvind.crud
package server

import slick.lifted.{Rep, Query}

trait tableMetadata extends astParsers {

  object Metadata {
    def base[ID: FlatRepShape, TABLE, P]
            (tableName: TableName,
             q:         Query[TABLE, P, Seq],
             idCol:     TABLE ⇒ Rep[ID],
             idCell:    Cell[ID],
             cr:        CellRow[P]): Metadata[ID, P] = {

      val IdName  = AstParser.colName(q)(idCol)
      val pkCell  = new PKCell(idCell)

      /* inject idCell in cell list, and lose the types */
      val cells = cellsWithColumnNames(q, cr) map {
        case (IdName,  _   ) ⇒ (IdName,  pkCell.asInstanceOf[Cell[Any]])
        case (colName, cell) ⇒ (colName, cell.asInstanceOf[Cell[Any]])
      }

      Metadata(tableName, cr.unpackValues, cr.packValues, cells, pkCell, IdName)
    }

    def derive[TABLE, P, OID, OP]
              (q:       Query[TABLE, P, Seq],
               origin:  Metadata[OID, OP],
               cellRow: CellRow[P]): Metadata[OID, P] = {

      /* prefer to keep override cells from origin */
      val cells = cellsWithColumnNames(q, cellRow) map {
        case (col, cell) ⇒
          origin.lookupCellFor(col) match {
            case Some(existing) ⇒ (col, existing)
            case _              ⇒ (col, cell.asInstanceOf[Cell[Any]])
          }
      }

      if (cells.length != cellRow.cells.length){
        throw new RuntimeException(s"Internal error while initializing slick-crud: Couldn't understand query: Only found columns $cells")
      }

      Metadata(origin.mainTable, cellRow.unpackValues, cellRow.packValues, cells, origin.idCell, origin.idCol)
    }
    
    private def cellsWithColumnNames[TABLE, P](q: Query[TABLE, P, Seq], cr: CellRow[P]): Seq[(ColumnRef, Cell[_])] =
      AstParser colNames q zip cr.cells
  }

  case class Metadata[ID, P](mainTable:    TableName,
                             unpackValues: P ⇒ List[Any],
                             packValues:   Seq[Any] ⇒ P,
                             cells:        Seq[(ColumnRef, Cell[Any])],
                             idCell:       Cell[ID],
                             idCol:        ColumnRef) {

    def lookupCellFor(col: ColumnRef): Option[Cell[Any]] =
      cells collectFirst {
        case (ci, c) if ci =:= col ⇒ c
      }

    def colNames: Seq[ColumnRef] =
      cells map (_._1)

    def extractIdFromRow(row: P): Option[ID] =
      colNames.indexOf(idCol) match {
        case -1  ⇒ None
        case idx ⇒ unpackValues(row)(idx).asInstanceOf[ID].some
      }
    
    def encodeId(id: ID): StrRowId = 
      (idCell encode id).asId

    def encodeRow(idOpt: Option[ID])(row: P): StrTableRow = {
      val idStrOpt  = idOpt orElse extractIdFromRow(row) map encodeId
      val strValues = cells zip unpackValues(row) map {
        case ((_, cell), value) ⇒ cell encode value
      }
      StrTableRow(idStrOpt, strValues)
    }

    def decodeId(v: StrRowId): ErrorMsg \/ ID =
      idCell decode v.asValue

    def decodeRow(params: Map[ColumnRef, StrValue]): Seq[(ColumnRef, ErrorMsg)] \/ P = {
      val colValues: Seq[(ColumnRef, ErrorMsg) \/ Any] = cells map {
        case (col, cell) ⇒
          if (col.isAutoInc) \/-(null) //YOLO
          else {
            val ret = params get col match {
              case Some(paramValue) ⇒ cell decode paramValue
              case _                ⇒ ErrorMsg(s"Didn't provide value").left
            }
            ret.leftMap(e ⇒ (col, e))
          }
      }
      sequence(colValues) map packValues
    }

    private def sequence[L, R](result: Iterable[L \/ R]): (Seq[L] \/ Seq[R]) =
      result.foldLeft[\/[Seq[L], Seq[R]]](\/-(Seq.empty)){
        case ( \/-(acc),  \/-(u)) ⇒  \/-(acc :+ u)
        case (-\/ (acc), -\/ (f)) ⇒ -\/ (acc :+ f)
        case (-\/ (acc),       _) ⇒ -\/ (acc)
        case (_        , -\/ (f)) ⇒ -\/ (Seq(f))
      }
  }
}
