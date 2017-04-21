# Ctor --------------------------------------------------------------------

#' A custom data type for Year-Quarter
#'
#' @param y year or a character string of a format like this: 2013-Q1
#' @param q quarter (optional)
#'
#' @return An object of type Quarter, character
#' @export
#'
#' @examples
#'
#' quarter(2013, 3)
#' quarter('2013-Q3')

date_yq <- function(y, q) {
  assert_that(is.numeric(y))
  assert_that(is.numeric(q))
  assert_that(all(q %in% 1:4))

  res <- as.integer(y) * 10L + as.integer(q)

  attr(res, 'class') <- c('date_yq', 'integer')
  res
}



# as_data_yq --------------------------------------------------------------

as_date_yq <- function(x){
  UseMethod('as_date_yq')
}


as_date_yq.numeric <- function(x){
  y <- x %/% 10
  q <- x %% 10
  date_yq(y = y, q = q)
}


as_date_yq.Date <- function(x){
  y <- lubridate::year(x)
  q <- lubridate::quarter(x)
  date_yq(y = y, q = q)
}



# as.Date -----------------------------------------------------------------



as.Date.date_yq <- function(x){
  y <- x %/% 10
  m <- c(1, 4, 7, 10)[x %% 10]
  lubridate::make_date(y, m, 1L)
}


# Format ------------------------------------------------------------------

#' Format a date_yq object
#'
#' @param x a [date_yq] object
#' @param format A scalar character, valid values are: `"iso"`, `"short"`, and `"shorter"`
#'
#' @return A character vector
#'
#' @md
#' @export
#' @examples
#'
#' x <- date_yq(2015, 3)
#'
#' format(x, format = "iso")
#' # [1] "2015-Q3"
#'
#' format(x, format = "short")
#' # [1] "2015.3"
#'
#' format(x, format = "shorter")
#' # [1] "15.3"
#'
format.date_yq <- function(
  x,
  format = 'iso'
){
  switch(
    tolower(format),
    "iso"     = format_date_yq_iso(x),
    "short"   = format_date_yq_short(x),
    "shorter" = format_date_yq_shorter(x),
    stop('wrong format specified')
  )
}

format_date_yq_iso <- function(x){
  y <- x %/% 10
  q <- x %% 10
  sprintf("%s-Q%s", y, q)
}

format_date_yq_short <- function(x){
  y <- x %/% 10
  q <- x %% 10
  sprintf("%s.%s", y, q)
}

format_date_yq_shorter <- function(x){
  y <- (x %/% 10) %% 100
  q <- x %% 10
  sprintf("%s.%s", y, q)
}


# Utils -------------------------------------------------------------------

#' Format Year-Quarter
#'
#' A wrapper around [as_date_yq()] and [format.date_yq()].
#'
#' @param x Any class that can be handled by [as_date_yq()]
#' @inheritParams format.date_yq
#'
#' @return A character vector
#'
#' @md
#' @export
#' @examples
#'
#' format_yq(20151)
#' format_yq(20151, format = 'short')
#' format_yq(20151, format = 'shorter')
#'
format_yq <- function(x, format = 'iso'){
  res <- as_date_yq(x)
  format(res, format = format)
}

