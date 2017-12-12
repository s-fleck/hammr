#' Relative Difference Between Numeric Values
#'
#' @param x,y
#' @param fun see https://en.wikipedia.org/wiki/Relative_change_and_difference
#'
#' @return
#' @export
#'
#' @examples
relative_diff <- function(x, y, fun = function(.x, .y) (x + y) / 2 ){
    abs(x - y) / fun(x, y)
}
