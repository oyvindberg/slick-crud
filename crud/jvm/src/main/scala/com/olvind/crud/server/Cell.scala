package com.olvind.crud
package server

import java.sql.Timestamp
import java.text.SimpleDateFormat

import scala.reflect.ClassTag

/**
 * A Cell is the mapping of a type to/from the stringly typed web.
 */
sealed trait Cell[E] {
  val rendering:        CellRendering
  val isOptional:       Boolean
  val isEditable:       Boolean
  val typeName:         String
  def restrictedValues: Option[Seq[E]]

  private[server] val rawDecode: String ⇒ E
  private[server] val rawEncode: E ⇒ String

  final def encode(e: E): StrValue =
    StrValue(rawEncode(e))

  final def decode(str: StrValue): ErrorMsg \/ E =
    ErrorMsg.tryCatch(rawDecode(str.value)){
      th ⇒ s"«${str.value}» is not a valid $typeName: ${th.getMessage}"
    }
}

private[server] final class SimpleCell[E: ClassTag]
  (private[server] override val rawDecode:  String ⇒ E)
  (private[server] override val rawEncode:  E      ⇒ String,
                   override val rendering:  CellRendering,
                   override val isEditable: Boolean) extends Cell[E] {

  override val isOptional       = false
  override val typeName         = implicitly[ClassTag[E]].runtimeClass.getSimpleName
  override def restrictedValues = None
}

private[server] final class RestrictedCell[E]
  (             wrapped:          Cell[E])
  (override val restrictedValues: Option[Seq[E]]) extends Cell[E]{

  override val rendering  = wrapped.rendering
  override val isEditable = wrapped.isEditable
  override val isOptional = wrapped.isOptional
  override val typeName   = s"Enum[${wrapped.typeName}]"

  private[server] override val rawDecode = wrapped.rawDecode
  private[server] override val rawEncode = wrapped.rawEncode
}

private[server] final class PKCell[E](wrapped: Cell[E]) extends Cell[E] {
  override val rendering        = CellRendering.Link
  override val typeName         = s"PK[${wrapped.typeName}]"
  override val isEditable       = wrapped.isEditable
  override val isOptional       = wrapped.isOptional
  override val restrictedValues = None

  private[server] override val rawDecode = wrapped.rawDecode
  private[server] override val rawEncode = wrapped.rawEncode
}

private[server] final class OptionCell[E](wrapped: Cell[E]) extends Cell[Option[E]] {
  override val rendering        = wrapped.rendering
  override val isEditable       = wrapped.isEditable
  override val isOptional       = true
  override val typeName         = s"Option[${wrapped.typeName}]"
  override val restrictedValues = wrapped.restrictedValues.map(_.map(Option(_)))

  private[server] override val rawDecode        =
    (s: String) ⇒ s.trim.some filterNot (_.isEmpty) map wrapped.rawDecode
  private[server] override val rawEncode        =
    (oe: Option[E]) ⇒ oe map wrapped.rawEncode getOrElse ""
}

final class CellOps[E](private val c: Cell[E]) extends AnyVal {
  def xmap[F: ClassTag](to:         E ⇒ F)
                       (from:       F ⇒ E,
                        rendering:  CellRendering = CellRendering.Text,
                        isEditable: Boolean       = true): Cell[F] =
    Cell[E, F](to)(from, rendering, isEditable)(c, implicitly[ClassTag[F]])

  def restricted(es: Seq[E]): Cell[E] =
    new RestrictedCell[E](c)(es.some)
}

object Cell {
  val nonEmpty: String ⇒ String = _.ensuring(_.nonEmpty)

  def apply[E: Cell]: Cell[E] = implicitly

  def apply[E: Cell, F: ClassTag]
           (to:         E ⇒ F)
           (from:       F ⇒ E,
            rendering:  CellRendering   = CellRendering.Text,
            isEditable: Boolean         = true
           ): Cell[F] =
    new SimpleCell[F](to compose Cell[E].rawDecode)(
      from andThen Cell[E].rawEncode,
      rendering,
      isEditable
    )

  def instance[E: ClassTag](rawEncode:  String ⇒ E)
                           (rawDecode:  E      ⇒ String,
                            rendering:  CellRendering   = CellRendering.Text,
                            isEditable: Boolean         = true): Cell[E] =

    new SimpleCell[E](rawEncode)(rawDecode, rendering, isEditable)

  @inline implicit def optionCell[E](implicit wrapped: Cell[E]): Cell[Option[E]] =
    new OptionCell(wrapped)

  @inline implicit def cellOps[E](c: Cell[E]): CellOps[E] =
    new CellOps(c)

  implicit val cellBoolean      = instance[Boolean](_.toBoolean)(_.toString, rendering = CellRendering.Checkbox)
  implicit val cellChar         = instance[Char]   (_.apply(0)) (_.toString, rendering = CellRendering.Text)
  implicit val cellFloat        = instance[Float]  (_.toFloat)  (_.toString, rendering = CellRendering.Number)
  implicit val cellDouble       = instance[Double] (_.toDouble) (_.toString, rendering = CellRendering.Number)
  implicit val cellInt          = instance[Int]    (_.toInt)    (_.toString, rendering = CellRendering.Number)
  implicit val cellLong         = instance[Long]   (_.toLong)   (_.toString, rendering = CellRendering.Number)
  implicit val cellString       = instance[String] (nonEmpty)   (identity)

  /* The rest are here because `UpdateNofitier` needs them */

  private object timestamp{
    def dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss.SSS")
    def parse(s: String) = new java.sql.Timestamp(dateFormat.parse(s).getTime)
    def format(td: Timestamp) = dateFormat.format(td)
  }

  implicit val cellTimestamp    = instance[Timestamp](timestamp.parse)(timestamp.format)
  implicit val cellTableName    = instance[TableName ](TableName) (_.value)
  implicit val cellColName      = instance[ColumnName](ColumnName)(_.value)
  implicit val cellUserInfoName = instance[UserInfo  ](UserInfo)  (_.value)
  implicit val cellStrRowIdName = instance[StrRowId  ](StrRowId)  (_.value)
  implicit val cellStrValueName = instance[StrValue  ](StrValue)  (_.value)
}
