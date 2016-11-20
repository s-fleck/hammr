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

#' String concatenation infix operator
#'
#' %_% pastes two strings together (with " " as sepparator)
#' %-% pastes two strings together (without sepparator)
#'
#' @param a A string
#' @param b Another string
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
