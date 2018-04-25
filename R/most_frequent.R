#' Get the n most frequent values of a vector
#'
#' Warning: does not behave in a defined way if several values with the same
#' frequency occur in `x`. If you are looking for a true implementation of the
#' statistical mode, please refer to the \pkg{modeest} package.
#'
#' @param x A vector
#' @param n `integer` scalar. Number of most frequent elements to return
#' @param na.rm `logical` scalar. Should `NA` values be stripped from `x` before computation
#'
#' @return a vector of the `n` most frequent elements
#' @md
#'
#' @export
most_frequent <- function(
  x,
  n = 1L,
  na.rm = FALSE
){
  assert_that(is.scalar(n))

  if(na.rm){
    x <- x[!is.na(x)]
  }

  ux    <- unique(x)
  freqs <- tabulate(match(x, ux))

  if(n == 1L){
    ux[which.max(freqs)]
  } else {
    ux[order(freqs, decreasing = TRUE, method = 'radix')][seq.int(n)]
  }
}
