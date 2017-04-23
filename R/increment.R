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

increment <- function(x, inc = 1){
  UseMethod("increment", x)
}


#' @export
#' @rdname increment
increment.date_yq <- function(x, inc){
  x <- as.integer(x)
  quarters <- x %% 10 + inc %% 4
  q <- quarters %% 4
  y <- quarters %/%4 + inc %/% 4L + x %/% 10
  y[q == 0L] <- y[q == 0] - 1L
  q[q == 0L] <- 4L

  date_yq(y, q)
}
