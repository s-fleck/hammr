#' Relative Difference Between Numeric Values
#'
#' @param x,y
#' @param fun see https://en.wikipedia.org/wiki/Relative_change_and_difference
#'
#' @return
#' @export
#'
#' @examples
relative_diff <- function(x, y, fun = function(.x, .y) pmax(.x, .y) ){
    abs(x - y) / fun(x, y)
}
