#' Lapply by class
#'
#' Same as \code{\link{lapply}}, just that it only applies fun to elements of a specified
#' class.
#'
#' @param x
#' @param fun
#' @param classes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
lapply_by_class <- function(x, fun, classes, ...){

  inner_fun <- function(inner_x, ...){
    if(any(class(inner_x) %in% classes)){
      return(fun(inner_x, ...))
    } else {
      return(inner_x)
    }
  }

  lapply(x, inner_fun, ...)
}
