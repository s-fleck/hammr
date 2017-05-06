#' Rearrange vector based on priorities
#'
#' Shoves elements of a character or factor vector to the front or back.
#' Usefull for reordering factor levels for plotting. Throws
#' a warning if any elements of `high` or `low` are not present in `x`.
#'
#' @param x a character of factor vector
#' @param high elements to be put to the front
#' @param low elements to be put to the back
#'
#' @rdname prioritize
#'
#' @return a reordered vector
#' @export
#' @family vector tools
#'
#' @examples
#'
#' x <- c('d', 'e', 'z', 'y', 'n', 'b', 'c', 'a', 'x')
#' prioritize(x, c('a', 'b', 'c', 'applepie'), c('x', 'y', 'z'))
#'
vec_prioritize <- function (x, high, low) {
  UseMethod("vec_prioritize", x)
}


#' @rdname prioritize
#' @export
#'
vec_prioritise <- vec_prioritize


#' @rdname prioritize
#' @export
#'
vec_prioritize.default <- function(x, high = NULL, low = NULL){
  low_not_x  <- low[!low %in% x]
  high_not_x <- high[!high %in% x]

  if(!all(low  %in% x)) {
    warning(
      'Not all "low" are present in "x": ',
      paste(low_not_x, collapse = ' '))
  }
  if(!all(high %in% x)){
    warning(
      'Not all "high" are present in "x": ',
      paste(high_not_x, collapse = ' '))
  }

  low      <- low[low %in% x]
  high     <- high[high %in% x]
  mid      <- x[!x %in% c(high, low)]
  ordered  <- c(high, mid, low)

  return(ordered)
}


#' @rdname prioritize
#' @export
#'
vec_prioritize.factor <- function(x, high = NULL, low = NULL){
  ordered <- vec_prioritize(levels(x), high, low)
  res     <- factor(x, levels = ordered)
  return(res)
}
