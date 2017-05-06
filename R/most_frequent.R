#' Get the n most frequent values of a vector
#'
#' Warning: does not behave in a defined way if several values with the same
#' frequency occur in `x`. If you are looking for a true implementation of the
#' statistical mode, pleasre refer to the [modeest] package.
#'
#' @param x A vector
#' @param n number of most frequent elements to get
#'
#' @return a character vector of the `n` most frequent elements
#' @md
#'
#' @export
most_frequent <- function(x, n = 1L, na.rm = FALSE){
  if(na.rm){
    x <- x[!is.na(x)]
  }

  ux    <- unique(x)
  freqs <- tabulate(match(x, ux))

  if(n == 1L){
    return(ux[which.max(freqs)])
  } else {
    return(ux[order(freqs, decreasing = TRUE, method = 'radix')][seq.int(n)])
  }
}
