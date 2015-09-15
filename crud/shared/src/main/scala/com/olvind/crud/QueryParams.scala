package com.olvind.crud

case class PageNum(value: Int) extends AnyVal {
  def previous = copy(value = value - 1)
  def next     = copy(value = value + 1)
}

object PageNum {
  val zero = PageNum(0)
}

case class Filter(columnInfo: ColumnRef, value: StrValue)

case class QueryParams(pageSize: Int, 
                       page:     PageNum, 
                       sorting:  Option[(ColumnRef, SortOrder)],
                       filter:   Option[Filter]) {
  
  def start = (pageSize * page.value) + 1 //one-based indices
  def end   = start + pageSize

  def withSortedBy(C: ColumnRef): QueryParams =
    sorting match {
      case Some((C, Asc)) ⇒ copy(sorting = (C, Desc).some)
      case _              ⇒ copy(sorting = (C, Asc).some)
    }
  
  def withNextPage: QueryParams =
    copy(page = page.next)

  def withFilter(of: Option[Filter]): QueryParams =
    copy(filter = of)
}

object QueryParams{
  val defaultPageSize: Int = 50
}
