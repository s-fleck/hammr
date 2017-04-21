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


as.Date.date_yq <- function(x){
  y <- x %/% 10
  m <- c(1, 4, 7, 10)[x %% 10]
  lubridate::make_date(y, m, 1L)
}


format.date_yq <- function(
  x,
  format = 'iso'
){
  switch(
    tolower(format),
    "iso"    = date_yq_to_iso(x),
    "short"   = date_yq_to_short(x),
    "shorter" = date_yq_to_shorter(x),
    stop('wrong format specified')
  )
}


date_yq_to_iso <- function(x){
  y <- x %/% 10
  q <- x %% 10
  sprintf("%s-Q%s", y, q)
}

date_yq_to_short <- function(x){
  y <- x %/% 10
  q <- x %% 10
  sprintf("%s.%s", y, q)
}

date_yq_to_shorter <- function(x){
  y <- (x %/% 10) %% 100
  q <- x %% 10
  sprintf("%s.%s", y, q)
}

