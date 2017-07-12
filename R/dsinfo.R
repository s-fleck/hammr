#' Set and Display Info about a Data Set
#'
#' @param x any R object
#'
#' @return
#' @export
#'
dsinfo <- function(x){
  attr(x, 'dsinfo')
}




#' @param version Version of the Dataset (after your own versionen scheme)
#' @param comment A comment on / description of the dataset.
#' @param reporting_period Reporting Period of the data set. Must be a [date_xx]
#'   object. [lubridate::period()] objects will likely be supported in the future.
#' @param ... any number of further objects to be stored in the `dsinfo`
#'   attribute.
#'
#' @return `set_dsinfo()` attaches the attribute `dsinfo` to an object.
#'
#' @rdname dsinfo
#' @export
#'
set_dsinfo <- function(
  x,
  version = NULL,
  comment = NULL,
  reporting_period = NULL,
  ...
){
  info <- c(
    list(
      version = version,
      reporting_period = reporting_period,
      comment = comment
    ),
    list(...)
  )

  class(info) <- c('dsinfo', 'list')
  attr(x, 'dsinfo') <- info

  return(x)
}




# reporting_period --------------------------------------------------------



#' @return `reporting_period()` retrieves the `reporting_period` field of the
#'   `dsinfo` attribute of `x` (or `NULL` if no such attribute exists)
#'
#' @rdname dsinfo
#' @export
#'
reporting_period <- function(x){
  if(is.null(attr(x, 'dsinfo'))){
    return(NULL)
  } else {
    attr(x, 'dsinfo')$reporting_period
  }
}




#' @param value a [hammr::date_xx] object
#'
#' @rdname dsinfo
#' @export
`reporting_period<-` <- function(x, value){
  assert_that(is_date_xx(value))
  x <- set_dsinfo(x, reporting_period = value)
  x
}




#' @param y,q,m integer. year, quarter, month. Month and quarter are optional,
#'  and mutually exclusive (only supply one, not both)
#'
#' @return `set_reporting_period()` and `'reporting_period<-'` can be used to
#'   directlty set the `reporting_period` field of the `dsinfo` attribute of
#'   an R object.
#'
#' @rdname dsinfo
#' @export
#'
set_reporting_period <- function(x, y, q = NULL, m = NULL){
  value <- make_date_xx(y, q, m)
  x <- set_dsinfo(x, reporting_period = value)
}





#' @return `has_reporting_period()` returns `TRUE` if `x` has a valid
#'   `reporting_period`, and `FALSE` otherwise
#'
#' @rdname dsinfo
#' @export
#'
has_reporting_period <- function(x){
  hammr::is_date_xx(reporting_period(x))
}
