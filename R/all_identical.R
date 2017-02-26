#' Test if all elements of a vector are identical
#'
#' @param x vector to be tested
#' @param empty_value Value to return if all_identical is called on a vector of length 0
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#'
#' all_identical(c(1,2,3))
#' all_identical(c(1,1,1))
#'

all_identical <- function(x, empty_value = FALSE) {
  # Check inputs
  if(length(x) <= 1L){
    if(identical(length(x), 1L)){
      warning("'x' consists of only one element")
      return(TRUE)
    } else if (identical(length(x), 0L)){
      if(is.null(x)){
        warning("'x' is NULL")
      } else {
        warning("'x' is an empty vector")
      }
      return(empty_value)
    }
  } else {
    identical(length(unique(x)), 1L)
  }
}
