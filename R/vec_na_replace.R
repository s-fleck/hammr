#' Replace na values in a vector
#'
#'
#' @param x An input vector.
#' @param interval Value to replace NAs with. Must be of the same type as `x`
#'
#' @md
#' @family vector tools
#' @export
vec_na_replace <- function(x, replace){
  dplyr::case_when(
    is.na(x) ~ replace,
    TRUE  ~ x
  )
}
