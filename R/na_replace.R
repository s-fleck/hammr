#' Replace na values in a vector
#'
#' @param replace
#' @param inf
#' @param x An input vector.
#'
#' @md
#' @family vector tools
#' @export
#'
na_replace <- function(x, replace, inf = FALSE, ...){
  assert_that(is.scalar(replace))
  assert_that(is.flag(inf))
  UseMethod('na_replace')
}



na_replace.default <- function(x, replace, inf = FALSE, ...){

  if(!inf){
    x[is.na(x)] <- replace
  } else {
    x[is.na(x) | is.infinite(x)] <- replace
  }

  x
}



na_replace.data.frame <- function(x, replace, inf, ...){
  df_na_replace(x, replace, inf, ...)
}
