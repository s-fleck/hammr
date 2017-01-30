#' Check if an object is valid
#'
#' requires that is_valid.class is defined somewhere
#'
#' @param x an R object
#'
#' @return logical; whether this object meets pre-defined validity conditions.
#' @export
#' @rdname is_valid
is_valid <- function(x, ...) {
  UseMethod("is_valid")

  print('test')
}



#' @export
#' @rdname is_valid
assert_valid <- function(dat, ...){

  v <- is_valid(dat)

  if(v){
    return(TRUE)
  } else {
    stop(assert_valid_error(dat))
  }
}



assert_valid_error  <- function(obj) {
  msg <- sprintf(
    'A validity check failed for object of class: %s.',
    paste(class(obj), collapse = ', ')
  )

  condition(c('assert_valid_error', 'error'),
            message = msg)
}
