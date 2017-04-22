#' Test Objects for Exact Equality
#'
#' Infix wrapper for [base::identical()]
#'
#' @param x any R object
#' @param y any R object
#'
#' @md
#' @export
`%identical%` <- function(x, y){
  identical(x, y)
}
