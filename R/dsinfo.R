#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
dsinfo <- function(x){
  attr(x, 'dsinfo')
}




#' Title
#'
#' @param version
#' @param comment
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
set_dsinfo <- function(
  x,
  version = NULL,
  comment = NULL,
  reporting_period = NULL,
  ...
){
  attr(x, 'dsinfo') <- c(
    list(
      version = version,
      reporting_period = reporting_period,
      comment = comment
    ),
    list(...)
  )

  return(x)
}




# reporting_period --------------------------------------------------------

#' Access and Modify Reporting Periods of R Object
#'
#' @param x Any R object
#'
#' @return `reporting_period` attribute of `x`
#' @export
#'
reporting_period <- function(x){
  if(is.null(attr(x, 'dsinfo'))){
    return(NULL)
  } else {
    attr(x, 'dsinfo')$reporting_period
  }
}




#' @param a [hammr::date_xx] object
#'
#' @rdname set_reporting_period
#' @export
`reporting_period<-` <- function(x, value){
  assert_that(is_date_xx(value))
  x <- set_dsinfo(x, reporting_period = value)
  x
}




#' @param y integer. a year
#' @param q integer. a quarter (optional, only supply month or quarter, not both)
#' @param m integer. a month (optional, only supply month or quarter, not both)
#'
#' @return `set_reporting_period()` and `'reporting_period<-'` can be used to
#'   modify the reporting period attribute of an R object.
#'
#' @export
#' @rdname reporting_period
#'
set_reporting_period <- function(x, y, q = NULL, m = NULL){
  value <- as_reporting_period(y, q, m)
  x <- set_dsinfo(x, reporting_period = value)
}




#' @rdname reporting_period
#'
#' @return `has_reporting_period()` returns `TRUE` if `x` has a valid
#'   `reporting_period` attribute, and `FALSE` otherwise
#' @export
#'
has_reporting_period <- function(x){
  hammr::is_date_xx(reporting_period(x))
}




#' @return `as_reporting_period()` is a helper that returns a valid `date_xx` object for
#'   assignment as reporting period.
#' @rdname reporting_period
#'
as_reporting_period <- function(y, q = NULL, m = NULL){
  if(!is.null(q)){
    assert_that(is.null(m))
    date_yq(y, q)
  } else if (!is.null(m)){
    date_ym(y, m)
  } else {
    date_y(y)
  }
}
