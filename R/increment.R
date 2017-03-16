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
increment.Quarter <- function(x, inc){
  res <- x %>%
    as.data.frame() %>%
    dplyr::mutate(
      y = as.integer(y),
      q = as.integer(q)
    )

  res$q <- res$q + inc

  while(any(res$q > 4, na.rm = TRUE)){
    res$y[res$q > 4] <- res$y[res$q > 4] + 1
    res$q[res$q > 4] <- res$q[res$q > 4] - 4
  }

  while(any(res$q < 1, na.rm = TRUE)){
    res$y[res$q < 1] <- res$y[res$q < 1] - 1
    res$q[res$q < 1] <- res$q[res$q < 1] + 4
  }

  quarter(res$y, res$q)
}
