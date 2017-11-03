#' Add S3 Subclass
#'
#' @param x Any \R object
#' @param class a character vector
#'
#' @return `x` with additional s3 class(es)
#' @export
#'
#' @examples
#' x <- iris
#' class(x)
#'
#' x <- add_subclass(x, "iris-example")
#' class(x)
#'
add_subclass <- function(x, class){
  class(x) <- union(class, class(x))
  x
}

