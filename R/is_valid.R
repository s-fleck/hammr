#' Check if an object is valid
#'
#' requires that is_valid.class is defined somewhere
#'
#' @param x an R object
#'
#' @return logical; whether this object meets pre-defined validity conditions.
#' @export
is_valid <- function(x, ...) {
  UseMethod("is_valid")
}

#' @export
on_failure(is_valid) <- function(call, env){
  cls <- class(deparse(call$x))
  cls <- paste(cls, collapse = ', ')
  paste("A validity check failed for object of class", cls)
}



