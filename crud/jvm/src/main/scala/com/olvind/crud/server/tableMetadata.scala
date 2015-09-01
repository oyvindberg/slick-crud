package com.olvind.crud
package server

import com.olvind.stringifiers.{DecodeFail, RenderHint, Stringifier}
import slick.lifted.{Query, Rep}

trait tableMetadata extends astParsers {

  object Metadata {
    def base[ID: FlatRepShape, TABLE, P]
            (tableName:     TableName,
             q:             Query[TABLE, P, Seq],
             idCol:         TABLE ⇒ Rep[ID],
             idStringifier: Stringifier[ID],
             stringRow:     StringifierRow[P]): Metadata[ID, P] = {

      val IdName        = AstParser.colName(q)(idCol)
      val pkStringifier = idStringifier.withFormat(RenderHint.Uri)

      /* inject pkStringifier in stringifier list, and lose the types */
      val cols: Seq[(ColumnRef, Stringifier[Any])] =
        StringifiersForCols(q, stringRow) map {
          case (IdName,  _   )       ⇒ (IdName,  pkStringifier.asInstanceOf[Stringifier[Any]])
          case (colName, stringifier) ⇒ (colName, stringifier.asInstanceOf[Stringifier[Any]])
        }

      Metadata(tableName, stringRow.unpackValues, stringRow.packValues, cols, pkStringifier, IdName)
    }

    def derive[TABLE, P, OID, OP]
              (q:         Query[TABLE, P, Seq],
               origin:    Metadata[OID, OP],
               stringRow: StringifierRow[P]): Metadata[OID, P] = {

      /* prefer to keep override stringifiers from origin */
      val stringifiers: Seq[(ColumnRef, Stringifier[Any])] =
        StringifiersForCols(q, stringRow) map {
          case (col, cell) ⇒
            origin.stringifierFor(col) match {
              case Some(existing) ⇒ (col, existing)
              case _              ⇒ (col, cell.asInstanceOf[Stringifier[Any]])
            }
        }

      if (stringifiers.length != stringRow.stringifiers.length){
        throw new RuntimeException(s"Internal error while initializing slick-crud: Couldn't understand query: Only found columns $stringifiers")
      }

      Metadata(origin.mainTable, stringRow.unpackValues, stringRow.packValues, stringifiers, origin.idStringifier, origin.idCol)
    }

    private def StringifiersForCols[TABLE, P](q: Query[TABLE, P, Seq], sr: StringifierRow[P]): Seq[(ColumnRef, Stringifier[_])] =
      AstParser colNames q zip sr.stringifiers
  }

  case class Metadata[ID, P](mainTable:     TableName,
                             unpackValues:  P ⇒ List[Any],
                             packValues:    Seq[Any] ⇒ P,
                             stringifiers:  Seq[(ColumnRef, Stringifier[Any])],
                             idStringifier: Stringifier[ID],
                             idCol:         ColumnRef) {

    def stringifierFor(col: ColumnRef): Option[Stringifier[Any]] =
      stringifiers collectFirst {
        case t@(`col`, stringifier) => stringifier
      }

    def colNames: Seq[ColumnRef] =
      stringifiers map (_._1)

    def extractIdFromRow(row: P): Option[ID] =
      colNames.indexOf(idCol) match {
        case -1  ⇒ None
        case idx ⇒ unpackValues(row)(idx).asInstanceOf[ID].some
      }

    def encodeId(id: ID): StrRowId =
      StrRowId(idStringifier encode id)

    def encodeRow(idOpt: Option[ID])(row: P): StrTableRow = {
      val idStrOpt  = idOpt orElse extractIdFromRow(row) map encodeId
      val strValues = stringifiers zip unpackValues(row) map {
        case ((_, stringifier), value) ⇒ StrValue(stringifier encode value)
      }
      StrTableRow(idStrOpt, strValues)
    }

    def decodeRow(idStrOpt: Option[StrRowId], values: Map[ColumnRef, StrValue]): XRes[P] = {
      val parsedValues: Seq[Either[(ColumnRef, Option[DecodeFail]), Any]] =
        stringifiers map {
          case (columnRef, stringifier) ⇒
              values get columnRef match {
                case Some(paramValue) ⇒
                  stringifier decode paramValue.value match {
                    case Left(fail) => Left((columnRef, fail.some))
                    case Right(any) => Right(any)
                  }
                case None ⇒
                  if (columnRef.isAutoInc) Right(null) //YOLO
                  else                     Left((columnRef, None))
              }
        }
      sequence(parsedValues).right map packValues match {
        case Right(p) => XSuccess(p)
        case Left(fs) => XValidation(idStrOpt, fs)
      }
    }

    private def sequence[L, R](result: Iterable[Either[L, R]]): (Either[Seq[L], Seq[R]]) =
      result.foldLeft[Either[Seq[L], Seq[R]]](Right(Seq.empty)){
        case (Right(acc), Right(u)) ⇒ Right(acc :+ u)
        case (Left(acc),  Left(f) ) ⇒ Left(acc :+ f)
        case (Left(acc),  _       ) ⇒ Left(acc)
        case (_,          Left(f) ) ⇒ Left(Seq(f))
      }
  }
}
