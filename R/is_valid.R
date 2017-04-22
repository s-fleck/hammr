#' Check if an object is valid
#'
#' `is_valid()` is a gernic used throughout my packages to verify whether an
#' S3 objects matches certain criteria. `assert_valid()` is a wrapper for
#' `is_valid()` that raises an error and outputs an informative error message
#' if those criteria are violated.
#'
#' @param x an R object. See `methods(is_valid)`.
#' @param ... passed on to methods.
#'
#' @return `logical`; whether this object meets pre-defined validity conditions.
#'   An object returned by `is_valid()` may have an `errors` attribute that is
#'   a character vector containing all violations of the validity criteria.
#'
#' @md
#' @seealso [as_validation()]
#' @export
#' @rdname is_valid
is_valid <- function(x, ...) {
  UseMethod("is_valid")
}



#' @export
#' @rdname is_valid
assert_valid <- function(x, ...){
  res <- is_valid(x, ...)
  assert_that(is.flag(res))

  if(res){
    return(TRUE)
  } else {
    stop(assert_valid_error(x, errors = attr(res, 'errors')))
  }
}




# Utils -------------------------------------------------------------------

#' Helper for is_valid output
#'
#' This is a helper for construction `is_valid` methods. It takes a list of
#' logical values, checks if all are true, and attaches an `error` attribute
#' to the resulting logical value. That error attriute is used by
#' [assert_valid()] to construct informative error messages.
#'
#' @param x A named list of logical values.
#'
#' @return A logical scalar with an 'errors' attribute.
#'
#' @md
#' @seealso [is_valid()]
#' @export
#'
#' @examples
#'
#' x <- list(
#'   is_numeric = is.numeric(1.2),
#'   is_integer = is.integer(1.2)
#' )
#'
#' as_validation(x)
#'
#' # [1] FALSE
#' # attr(,"errors")
#' # [1] "is_integer"
#'
as_validation <- function(x){
  assert_that(is.list(x))

  chk <- unlist(x)
  res <- all(chk)
  attr(res, 'errors') <- names(unlist(chk))[-which(unlist(chk))]
  res
}




# Conditions --------------------------------------------------------------

assert_valid_error <- function(obj, errors = attr(obj, 'errors')){
  msg <- sprintf(
    'A validity check failed for object of class: %s.',
    paste(class(obj), collapse = ', ')
  )

  if(length(errors) > 0){
    msg <- sprintf('%s\nErrors: %s', msg, paste(errors, collapse = '\n'))
  }

  return(error('assert_valid_error', message = msg, call = sys.call(-2)))
}
