#' Test if all elements of a vector are identical
#'
#' http://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
#'
#' @param x vector to be tested
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#'
#' all_identical(c(1,2,3))
#' all_identical(c(1,1,1))
#'
all_identical <- function(x, warn_single_value = FALSE) {
  if (length(x) == 1L) {
    if(warn_single_value) warning("'x' has a length of only 1")
    return(TRUE)
  } else if (length(x) == 0L) {
    warning("'x' has a length of 0")
    return(logical(0))
  } else {
    TF <- vapply(1:(length(x)-1),
                 function(n) identical(x[[n]], x[[n+1]]),
                 logical(1))
    if (all(TF)) TRUE else FALSE
  }
}
