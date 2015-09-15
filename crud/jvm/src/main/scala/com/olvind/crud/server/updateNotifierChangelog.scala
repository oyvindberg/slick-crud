package com.olvind.crud
package server

import java.sql.Timestamp

trait updateNotifierChangelog extends dbIntegration {
  trait UpdateNotifierChangelog extends UpdateNotifier {
    val changelogTableName = "crud_changelog"

    import driver.api._

    implicit val m0 = MappedColumnType.base[TableName,  String](_.value, TableName)
    implicit val m1 = MappedColumnType.base[ColumnName, String](_.value, ColumnName)
    implicit val m2 = MappedColumnType.base[UserInfo,   String](_.value, UserInfo)
    implicit val m3 = MappedColumnType.base[StrValue,   String](_.value, StrValue)
    implicit val m4 = MappedColumnType.base[StrRowId,   String](_.value, StrRowId)
    implicit val m5 = MappedColumnType.base[EditorId,   String](_.value, EditorId)

    class ChangelogT(t: Tag) extends Table[(Long, EditorId, TableName, ColumnName, StrRowId, Option[StrValue], StrValue, Timestamp, UserInfo)](t, changelogTableName){
      def id           = column[Long]      ("id", O.PrimaryKey, O.AutoInc)
      def editor       = column[EditorId]  ("editor_id")
      def table        = column[TableName] ("table_name")
      def col          = column[ColumnName]("column_name")
      def row          = column[StrRowId]  ("row_id")
      def from         = column[StrValue]  ("from_value").?
      def to           = column[StrValue]  ("to_value")
      def timestamp    = column[Timestamp] ("changed_at")
      def userDetails  = column[UserInfo]  ("user_details")

      def * = (id, editor, table, col, row, from, to, timestamp, userDetails)
    }
    val Changelog = TableQuery[ChangelogT]

    override abstract def notifySuccess(user: UserInfo)(s: CrudSuccess) = {
      super.notifySuccess(user)(s)
      s match {
        case Updated(editor, col, row, from, to) ⇒
          db.run(Changelog += ((0L, editor, col.table, col.name, row, from, to, new Timestamp(System.currentTimeMillis()), user)))
          ()
        case _ ⇒ ()
      }
    }
  }
}
