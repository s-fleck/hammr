# Ctor --------------------------------------------------------------------

#' A custom data type for Year-Quarter
#'
#' @param y year
#' @param q quarter (optional)
#'
#' @return An object of type Quarter, character
#' @export
#'
#' @examples
#'
#' date_yq(2013, 3)
#'
date_yq <- function(y, q) {
  assert_that(is.numeric(y))
  assert_that(is.numeric(q))
  assert_that(all(q %in% 1:4))

  s <- ifelse(sign(y) >= 0, 1L, -1L)
  res <- (as.integer(abs(y)) * 10L + as.integer(q)) * s

  attr(res, 'class') <- c('date_yq')
  res
}



# as_data_yq --------------------------------------------------------------

#' Convert object to date_yq
#'
#' @param x any R object
#'
#' @md
#' @return a [date_yq] object
#' @export
#'
as_date_yq <- function(x){
  UseMethod('as_date_yq')
}



#' @export
as_date_yq.numeric <- function(x){
  assert_that(all(x > 0 | x <= -11L))
  d <- yqs_matrix_from_numeric(x)
  date_yq(y = d[, 1] * d[, 3], q = d[, 2])
}



#' @export
as_date_yq.Date <- function(x){
  y <- lubridate::year(x)
  q <- lubridate::quarter(x)
  date_yq(y = y, q = q)
}



# as.Date -----------------------------------------------------------------

#' Convert date_yq to Date
#'
#' @param x a [date_yq] object
#' @param ... ignored
#'
#' @return A [base::Date] object
#' @md
#' @export
#'
as.Date.date_yq <- function(x, ...){
  y <- year(x)
  m <- c(1, 4, 7, 10)[get_quarter(x)]
  lubridate::make_date(y, m, 1L)
}

is_date_yq <- function(x){
  inherits(x, 'date_yq')
}


# accessors ---------------------------------------------------------------

#' @export
year.date_yq <- function(x){
  as.integer(x) %/% 10
}

#' @export
get_quarter <- function(x){
  UseMethod('get_quarter')
}

#' @export
get_quarter.date_yq <- function(x){
  as.integer(x) %% 10
}

#' @export
get_quarter.Date <- function(x){
  .Deprecated()
  lurbidate::quarter(x)
}


#' @export
month.date_yq <- function(x){
  c(1, 4, 7, 10)[get_quarter(x)]
}


quarter_first <- function(x){
  UseMethod('quarter_first')
}


#' @export
first_day_yq <- function(y, q){
  date_yq(y, q) %>%
    as.Date()
}

#' @export
last_day_yq <- function(y, q){
  date_yq(y, q) %>%
    as.Date() %>%
    lubridate::ceiling_date('quarter') - 1L
}



# Format ------------------------------------------------------------------

#' Format a date_yq object
#'
#' @param x a [date_yq] object
#' @param format A scalar character, valid values are: `"iso"`, `"short"`, and
#'   `"shorter"`
#' @param ... ignored
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
  format = 'iso',
  ...
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
  d <- yqs_matrix_from_numeric(x)
  sprintf("%s-Q%s", d[, 1] * d[, 3], d[, 2])
}

format_date_yq_short <- function(x){
  d <- yqs_matrix_from_numeric(x)
  sprintf("%s.%s", d[, 1] * d[, 3], d[, 2])
}

format_date_yq_shorter <- function(x){
  d <- yqs_matrix_from_numeric(x)
  y <- stringi::stri_sub(as.character(d[, 1]), -2, -1)
  y <- ifelse(d[, 3] < 0, paste0('-', y), y)
  sprintf("%s.%s", y, d[, 2])
}

`+.date_yq` <- function(x, y){
  increment(as.integer(x), as.integer(y))
}

`-.date_yq` <- function(x, y){
  increment(as.integer(x), as.integer(-y))
}

`*.date_yq` <- function(x, y){
  stop('Operation not supported')
}

`/.date_yq` <- function(x, y){
  stop('Operation not supported')
}

`^.date_yq` <- function(x, y){
  stop('Operation not supported')
}

`%%.date_yq` <- function(x, y){
  stop('Operation not supported')
}

`%/%.date_yq` <- function(x, y){
  stop('Operation not supported')
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



yqs_matrix_from_numeric <- function(x){
  matrix(
    c(abs(x) %/% 10, q = abs(x) %% 10, s = sign(x)),
    ncol = 3
  )
}



