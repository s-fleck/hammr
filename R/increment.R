#' Increment functions for various self-defined objects
#'
#' Function to increment objects
#'
#' @param x object to increment
#' @param inc Value by which to increment (usually integer)
#'
#' @return An object of the same type increment by inc
#' @export
#' @rdname increment
#'
#' @examples
#'
#' quarter(2013, 2) %+% 2
increment <- function(x, inc = 1) UseMethod("increment")


