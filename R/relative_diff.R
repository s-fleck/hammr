#' Relative Difference Between Numeric Values
#'
#' `relative_difference` answers the question "by how much do `x` and `y` differ
#' from each other". The order of `x` and `y` is not important, the result will
#' always be positive, and 1\% difference (`0.01`) corresponds to 1/100th of
#' either `x` or `y`, whichever is bigger (with the default `fun`).
#'
#'
#' @param x,y `numeric` vectors. For `relative_change()` the order of `x` and
#'   `y` matters: `x` is the old value, and `y` is the new value.
#' @param fun a `function`. Sensible options options are (for example):
#'   * [`pmax`][pmax],
#'   * [`pmin`][pmin], and
#'   * `function(.x, .y) (.x + .y) / 2`
#'
#' @return a `numeric` vector
#' @seealso \url{https://en.wikipedia.org/wiki/Relative_change_and_difference}
#' @export
#'
#' @examples
#' relative_diff(c(99, 100), c(100, 101))
#'
relative_diff <- function(x, y, fun = function(.x, .y) pmax(.x, .y) ){
  abs(x - y) / fun(x, y)
}



#' @description
#' `relative_change` answers the question "by how much is has `x` changed to
#' become `y`" with `x` as the reference value (a result of `0.01` corresponds
#' to `x/100`)
#'
#' @export
#' @rdname relative_diff
#' @examples
#' relative_change(c(99, 100), c(100, 99))
#'
relative_change <- function(x, y){
  x / y - 1
}

