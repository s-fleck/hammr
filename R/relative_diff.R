#' Relative Difference Between Numeric Values
#'
#' @param x,y `numeric` vectors
#' @param fun a `function`. see https://en.wikipedia.org/wiki/Relative_change_and_difference
#'
#' @return a `numeric` vector
#' @export
#'
#' @examples
#'
#' relative_diff(c(99, 100), c(100, 101))
#'
relative_diff <- function(x, y, fun = function(.x, .y) pmax(.x, .y) ){
    abs(x - y) / fun(x, y)
}
