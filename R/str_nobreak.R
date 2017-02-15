#' Remove linebreaks and multiple spaces from string
#'
#' @param x a character vector.
#'
#' @return a character vector without linebreaks
#' @export
#'
#' @examples
#'
#' str_nobreak(
#'   blubb
#'   bar,
#'   foo'
#' )
#'
str_nobreak <- function(x){
  y <- gsub("\r?\n|\r", " ", x)
  gsub('[ ]{2,}', ' ', y)
}
