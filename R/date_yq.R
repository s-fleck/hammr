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

  attr(res, 'class') <- c('date_yq', 'integer')
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

  y <- abs(x) %/% 10 * sign(x)
  q <- abs(x) %% 10
  date_yq(y = y, q = q)
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
  y <- x %/% 10
  m <- c(1, 4, 7, 10)[x %% 10]
  lubridate::make_date(y, m, 1L)
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
  d <- yqs_matrix_from_nueric(x)
  sprintf("%s-Q%s", d[, 'y'] * d[, 's'], d[, 'q'])
}

format_date_yq_short <- function(x){
  d <- yqs_matrix_from_nueric(x)
  sprintf("%s.%s", d[, 'y'] * d[, 's'], d[, 'q'])
}

format_date_yq_shorter <- function(x){
  d <- yqs_matrix_from_nueric(x)
  y <- stringi::stri_sub(as.character(d[, 'y']), -2, -1)
  y <- ifelse(d[, 's'] < 0, paste0('-', y), y)
  sprintf("%s.%s", y, d[, 'q'])
}

yqs_matrix_from_nueric <- function(x){
  matrix(
    c(abs(x) %/% 10, q = abs(x) %% 10, s = sign(x)),
    ncol = 3,
    dimnames = list(NULL, c('y', 'q', 's'))
  )
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

