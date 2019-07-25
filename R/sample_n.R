#' Sample rows from a data frame
#'
#' @param x a `data.frame`
#' @param ... passed on to methods
#' @export
#'
sample_n <- function(x, ...){
  UseMethod("sample_n")
}




#' @rdname sample_n
#' @inheritParams base::sample
#' @return a `data.frame` with `size` rows
#' @export
sample_n.data.frame <- function(
  x,
  size = nrow(x),
  replace = FALSE,
  prob = NULL,
  ...
){
  x[sample(seq_len(nrow(x)), size = size, replace = replace, prob = prob), ]
}
