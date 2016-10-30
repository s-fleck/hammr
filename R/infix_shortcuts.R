#' Equal and not not NA
#'
#' Works like ==, just that it treats NAs like normal values:
#'
#'   NA %==% NA returns TRUE instead of NA
#'   1  %==% NA returns FALSE instead of NA
#'
#' @export
`%==%` <- function(a, b){
  res <- a == b  | is.na(a) & is.na(b)
  res[is.na(a) & !is.na(b)] <- FALSE
  res
}


#' @export
`%identical%` <- identical


# example of how to redefine the + operator for
# string concattenation
#
# http://stackoverflow.com/questions/4730551/making-a-string-concatenation-operator-in-r
#
# `+` <- function (e1, e2) {
#   UseMethod("+")
# }
#
# `+.default` <- function (e1, e2) .Primitive("+")(e1, e2)
#
# `+.character` <- function(e1, e2) {
#   if(length(e1) == length(e2)) {
#     paste(e1, e2, sep = '')
#   } else stop('String Verctors of Different Lengths')
# }
#


#' String concatenation infix operator
#'
#' @param a A string
#' @param b Another string
#'
#' %_% pastes two strings together (with ' ' as sepparator)
#' %-% pastes two strings together (without sepparator)
#'
#' @return a and b pasted together
#' @export
#' @rdname paste_infix
#'
#' @examples
#'
#' "ra" %-% "ce" %_% "car"

`%_%` <- function(a, b) {
  paste(a, b)
}


#' @rdname paste_infix
#' @export
`%-%` <- function(a, b) {
  paste0(a, b)
}
