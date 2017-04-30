# ctor --------------------------------------------------------------------

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

#' Get years component of a date_yq.
#'
#' `year()` is there for consistency with [lubridate], `get_year()` is there
#' for consistency with other `get_` functions in hammr. `year` should be
#' prefered for consistency in function naming across packages.
#'
#' @param x a [date_yq] object
#' @family yq getters
#'
#' @export
#' @aliases year
#'
#' @examples
#'
#' \dontrun{
#' x <- date_yq(2016, 2)
#'
#' year(x)
#' get_year(x)
#' }
#'
get_year <- function(x){
  UseMethod('get_year')
}

get_year.date_yq <- function(x){
  as.integer(x) %/% 10
}


#' @export
#' @rdname get_year
year.date_yq <- function(x){
  get_year(x)
}

#' Get quarter component of a date_yq.
#'
#' `get_quarter()` extracts the quarter of a [date_yq] object.
#' [lubridate::quarter()] also works, because \pkg{hammr} exports a method for
#' [lubridate::month()]. This will have more overhead than using
#' `get_quarter()`, but should generally be prefered for consistency in
#' function naming across packages.
#'
#' @inheritParams lubridate::year
#' @seealso [lubridate::quarter()]
#' @aliases quarter
#' @family yq getters
#' @export
#'
#' @examples
#'
#' x <- date_yq(2016, 2)
#'
#' quarter(x)
#' get_quarter(x)
#'
get_quarter <- function(x){
  UseMethod('get_quarter')
}

#' @export
get_quarter.date_yq <- function(x){
  as.integer(x) %% 10
}


#' Get month component of a date_yq
#'
#' `get_month()` extracts the month of a date_yq object.
#' A method for [lubridate::month()] is also exported. This will have
#' slightly more overhead than using `get_month()`, but should generally be
#' prefered for consistency in function naming across packages.
#'
#' @param x a [date_yq] object.
#' @inheritParams lubridate::month
#' @rdname get_month
#' @aliases month
#' @seealso [lubridate::month()]
#' @family yq getters
#' @export
#'
#' @examples
#'
#' x <- date_yq(2016, 2)
#'
#' month(x)
#' month(x, label = TRUE)
#' get_month(x)
#'
get_month <- function(x){
  UseMethod('get_month')
}

#' @export
get_month.date_yq <- function(x){
  c(1, 4, 7, 10)[get_quarter(x)]
}


#' @export
#' @rdname get_month
month.date_yq <- function(x, label = FALSE, abbr = TRUE){
  lubridate::month(get_month(x), label = label, abbr = abbr)
}




# format ------------------------------------------------------------------

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


# algebra -----------------------------------------------------------------



`+.date_yq` <- function(x, y){
  increment(x, as.integer(y))
}

`-.date_yq` <- function(x, y){
  increment(x, as.integer(-y))
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


# shortcuts ---------------------------------------------------------------

#' Convenience functions for formatted Year-Quarter output
#'
#' A wrapper around [date_yq()] and [format.date_yq()].
#'
#' @param x Any class that can be handled by [as_date_yq()]
#' @param y numeric. a year
#' @param q numeric. a quarter (1-4)
#' @inheritParams format.date_yq
#'
#' @return A character vector
#'
#' @md
#' @export
#' @examples
#'
#' format_yq(2015, 1)
#' format_as_yq(20151, format = 'short')
#' format_as_yq(20151, format = 'shorter')
#'
format_yq <- function(y, q, format = 'iso'){
  date_yq(y, q) %>%
    format(res, format = format)
}

#' @rdname format_yq
#' @export
format_as_yq <- function(x, format = 'iso'){
  as_date_yq(x) %>%
    format(res, format = format)
}


#' Get first / last day of a quarter
#'
#' @param x Anything that can be coerced to a date with [base::as.Date()]
#'
#' @return a [Date]
#'
#' @rdname day_of_quarter
#' @md
#' @export
#' @examples
#'
#' first_day_of_quarter('2016-06-04')
#' last_day_of_quarter('2016-06-04')
#'
first_day_of_quarter <- function(x){
  UseMethod('first_day_of_quarter')
}



#' @rdname day_of_quarter
#' @export
first_day_of_quarter.default <- function(x){
  lubridate::floor_date(as.Date(x), 'quarter')
}



#' @rdname day_of_quarter
#' @export
last_day_of_quarter <- function(x){
  UseMethod('last_day_of_quarter')
}



#' @rdname day_of_quarter
#' @export
last_day_of_quarter.default <- function(x){
  lubridate::ceiling_date(as.Date(x), 'quarter') - 1L
}




# utils -------------------------------------------------------------------

yqs_matrix_from_numeric <- function(x){
  x <- unclass(x)
  matrix(
    c(abs(x) %/% 10, q = abs(x) %% 10, s = sign(x)),
    ncol = 3
  )
}
