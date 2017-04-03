#' Reorder character vector or levels of a factor based on priorities
#'
#' Shoves elements of a character or factor vector to the front.
#' Usefull for reordering factor levels for plotting. Issues
#' a warning if any elements of high or low are not present in x
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
prioritize <- function (x, ...) {
  UseMethod("prioritize", x)
}


#' @rdname prioritize
#' @export
#'
prioritise <- prioritize


#' @rdname prioritize
#' @export
#'
prioritize.character <- function(x, high = character(), low = character()){
  low_not_x  <- low[!low %in% x]
  high_not_x <- high[!high %in% x]

  if(!all(low  %in% x)) warning('Not all "low" are present in "x": ', paste(low_not_x, collapse = ' '))
  if(!all(high %in% x)) warning('Not all "high" are present in "x": ', paste(high_not_x, collapse = ' '))

  low      <- low[low %in% x]
  high     <- high[high %in% x]
  mid      <- x[!x %in% c(high, low)]
  ordered  <- c(high, mid, low)

  return(ordered)
}


#' @rdname prioritize
#' @export
#'
prioritize.factor <- function(x, high = character(), low = character()){
  ordered <- prioritise(levels(x), high, low)
  res     <- factor(x, levels = ordered)

  return(res)
}
