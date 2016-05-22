#' String concattenation with + operator
#'
#' http://stackoverflow.com/questions/4730551/making-a-string-concatenation-operator-in-r
#'
#' @param e1
#' @param e2
#'
#' @return
#' @export
#'
#' @examples
`+` <- function (e1, e2) {
  UseMethod("+")
}
