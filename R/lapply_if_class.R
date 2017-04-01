#' Lapply if class
#'
#' Simple wrapper for [lapply()], that just applies `FUN` to elements
#' of a specified class.
#'
#' @param X a vector (atomic or list) or an [expression] object. Other objects
#'  (including classed objects) will be coerced by [base::as.list()].
#' @param FUN a function to be applied to each element of `X`
#' @param CLASSES a character vector. `FUN()` will only be applied to objects
#'   whose class matches any of the strings provided in `CLASSES`.
#' @param ... other arguments passed on to `FUN`
#'
#' @md
#' @return a list
#' @export
lapply_if_class <- function(X, FUN, CLASSES, ...){
  assert_that(is.character(CLASSES))

  inner_fun <- function(inner_x, ...){
    if(any(class(inner_x) %in% CLASSES)){
      return(FUN(inner_x, ...))
    } else {
      return(inner_x)
    }
  }

  lapply(X = X, FUN = inner_fun, ...)
}
