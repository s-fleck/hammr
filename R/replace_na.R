#' Replace na values in a vector
#'
#' @param x An input vector.
#' @param replace value to replace `NAs` with.
#' @param inf logical. if `TRUE`, `inf` values are treated like `NAs`
#' @param ... passed on to methods
#'
#' @md
#' @family vector tools
#' @export
#'
replace_na <- function(
  x,
  replace,
  inf = FALSE,
  ...
){
  assert_that(is.scalar(replace))
  assert_that(is.flag(inf))
  UseMethod("replace_na")
}




#' @export
replace_na.default <- function(
  x,
  replace,
  inf = FALSE,
  ...
){
  if (!inf){
    x[is.na(x)] <- replace
  } else {
    x[is.na(x) | is.infinite(x)] <- replace
  }
  return(x)
}




#' @export
replace_na.data.frame <- function(
  x,
  replace,
  inf,
  ...
){
  df_replace_na(x, replace, inf, ...)
}
