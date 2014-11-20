package no.penger.crud

import java.util.UUID

import scala.xml.NodeSeq

trait viewHtml extends view with viewFormatHtml {

  /* context path */
  val ctx: String

  override def append(one: NodeSeq, two: NodeSeq) =
    one ++ two

  override def View[ID: Cell, ROW](base: String, tableName: TableName, isEditable: Boolean, id: ColumnName, namedCells: NamedCellRow[ROW]): View[ID, ROW] =
    ViewHtml(ctx + base, tableName, isEditable, id, namedCells)

  case class ViewHtml[ID: Cell, ROW](
    base:       String,
    tableName:  TableName,
    isEditable: Boolean,
    idCol:      ColumnName,
    namedCells: NamedCellRow[ROW]) extends View[ID, ROW] {

    /* generate a random id for the table we render, for frontend to distinguish multiple tables */
    val uniqueId    = tableName+UUID.randomUUID().toString.filter(_.isLetterOrDigit)

    override def renderCell(columnName: ColumnName, value: Any, cell: Cell[Any]): ElemFormat =
      <td>{
      if (idCol =:= columnName)
        <a href={base + "/" + cell.toStr(value)} class="btn-style">{cell.toStr(value)}</a>
      else if (cell.inputType == "checkbox")
        enabled(isEditable && cell.isEditable)(checkedCheckbox(cell.inputType == "checkbox" && (value == true || value == Some(true)))(
          <input type="checkbox"/>
        ))
      else
        enabled(isEditable && cell.isEditable)(<input
            class={if (cell.alignRight) "right" else "left"}
            type={cell.inputType}
            placeholder={cell.typeName}
            value={cell.toStr(value)}
            autocomplete="off"
          />)
        }
      </td>

    override def many(rows: Seq[(ID, ROW)]) =
      <div>
        <table id={uniqueId}>
          {header(showNew = true, showSeeAll = true)}
          <thead>
            <tr>{namedCells.colNames.map(name ⇒ <th class="columnHeader">{name}</th>)} </tr>
          </thead><tbody>{
            rows.zipWithIndex.map {
              case ((id, row), idx) ⇒ <tr db-id={Cell.toStr(id)} class={if (idx % 2 == 0) "even" else ""}>{
                namedCells.cellsWithUnpackedValues(row).map {
                  case ((colName, cell), value) ⇒ renderCell(colName, value, cell)
                }
              }</tr>
            }
          }</tbody></table>
        <script type="text/javascript">no.penger.crud.view('{base}', '#{uniqueId}')</script>
      </div>

    override def notFound(idOpt: Option[ID]) =
      idOpt match {
        case Some(id) ⇒ <h3>{s"Found no referenced $tableName for id ${Cell.toStr(id)}"}</h3>
        case None     ⇒ <h3>{s"Found no referenced $tableName"}</h3>
      }

    override def single(id: ID, row: ROW) = {
        <table id={uniqueId} db-id={Cell.toStr(id)}>
          {header(showNew = true, showSeeAll = true)}
          <thead><tr><th>Column</th><th>Value</th></tr></thead>
          {namedCells.cellsWithUnpackedValues(row).map{
            case ((name, cell), value) ⇒ <tr><td class="columnHeader">{name}</td>{renderCell(name, value, cell)
          }</tr>}}
        </table>
        <script type="text/javascript">{s"no.penger.crud.single('$base', '#$uniqueId')"}</script>
    }

    override def newPage = {
      <table id={uniqueId}>
        {header(showNew = false, showSeeAll = true, showSave = true)}
        <tbody> {
          namedCells.cells.map{
            case (name, cell) ⇒ <tr><th class="columnHeader">{name}</th>{renderEmptyCell(cell)}</tr>
          }
        }</tbody>
      </table>
      <script type="text/javascript">{s"no.penger.crud.neew('$base', '#$uniqueId')"}</script>
    }

    def renderEmptyCell(cell: Cell[Any]): ElemFormat =
      <td>
        <input
          type={cell.inputType}
          class={"form-control"}
          placeholder={cell.typeName}
          ></input>
      </td>

    def header(showNew: Boolean, showSeeAll: Boolean, showSave: Boolean = false) = {
      <caption class="columnHeader">
        <strong>{tableName}</strong>
        {if (showNew)    <a             class="btn-style" href={base + "/new"} >New</a>     else NodeSeq.Empty}
        {if (showSeeAll) <a             class="btn-style" href={base}          >See all</a> else NodeSeq.Empty}
        {if (showSave)   <a id="submit" class="btn-style" href="#"             >Save</a>    else NodeSeq.Empty}
      </caption>
    }

    def checkedCheckbox(isChecked: Boolean)(elem: xml.Elem) =
      if (isChecked) elem % xml.Attribute("checked", Seq(xml.Text("")), xml.Null) else elem

    def enabled(isEnabled: Boolean)(elem: xml.Elem) =
      if (isEnabled) elem else elem % xml.Attribute("disabled", Seq(xml.Text("")), xml.Null)
  }
}
