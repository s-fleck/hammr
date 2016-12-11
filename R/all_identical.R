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
    identical(length(unique(x)), 1L)
  }
}

# testthat::test_file(file.path(testthat::test_path(), 'test_all_identical.R'))




#
# all_identical2 <- function(x, empty_value = FALSE) {
#   # Check inputs
#   if(length(x) <= 1L){
#     if(identical(length(x), 1L)){
#       warning("'x' consists of only one element")
#       return(TRUE)
#     } else if (identical(length(x), 0L)){
#       if(is.null(x)){
#         warning("'x' is NULL")
#       } else {
#         warning("'x' is an empty vector")
#       }
#       return(empty_value)
#     }
#   } else {
#     all(vapply(seq_along(x),
#                  function(n) {
#                    identical(x[[1]], x[[n]])
#                  },
#                  TRUE))
#   }
# }
#
