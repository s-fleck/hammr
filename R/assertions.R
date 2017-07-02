#' Assert that an object has a specifc class
#'
#' @param dat any R object
#' @param class the class to be checked for
#'
#' @return either `TRUE` or raises an error
#' @export
#' @seealso [assertthat::assert_that()]
#' @md
#' @rdname assert_class
#' @examples
#'
#' x = data.frame()
#' assert_class(x, 'data.frame')
#'
#' @export
#' @rdname assert_class
assert_class <- function(dat, class){
  assert_that(inherits(dat, class))
}


#' @export
#' @rdname assert_class
`%assert_class%` <- function(dat, class){
  assert_class(dat = dat, class = class)
}
