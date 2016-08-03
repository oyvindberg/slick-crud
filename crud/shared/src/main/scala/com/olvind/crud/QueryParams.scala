package com.olvind.crud

final case class PageNum(value: Int) extends AnyVal {
  def previous: PageNum = copy(value = value - 1)
  def next:     PageNum = copy(value = value + 1)
}

object PageNum {
  val zero = PageNum(0)
}

final case class Filter(columnInfo: ColumnRef, value: StrValue)

final case class QueryParams(
  pageSize:   Int,
  page:       PageNum,
  sortingOpt: Option[(ColumnRef, SortOrder)],
  filterOpt:  Option[Filter]) {

  def start: Int =
    (pageSize * page.value) + 1 //one-based indices

  def withSortedBy(C: ColumnRef): QueryParams =
    sortingOpt match {
      case Some((C, Asc)) ⇒ copy(sortingOpt = (C, Desc).some)
      case _              ⇒ copy(sortingOpt = (C, Asc).some)
    }
  
  def withNextPage: QueryParams =
    copy(page = page.next)

  def withFilter(of: Option[Filter]): QueryParams =
    copy(filterOpt = of)
}

object QueryParams{
  val defaultPageSize: Int =
    50
  val default: QueryParams =
    QueryParams(defaultPageSize, PageNum.zero, None, None)
}
