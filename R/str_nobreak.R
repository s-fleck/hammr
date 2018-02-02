#' Remove linebreaks and multiple spaces from string
#'
#' @param x a character vector.
#' @param replace_linebreak a scalar character to replace linebreaks with
#'
#' @return a character vector with linebreaks and multiple consecutive spaces
#'   removed
#' @export
#'
#' @md
#' @family string tools
#' @examples
#'
#' str_nobreak(
#'   "blubb,
#'   bar,
#'   foo"
#' )
#'
str_nobreak <- function(x, replace_linebreak = " "){
  assert_that(rlang::is_scalar_character(replace_linebreak))
  y <- gsub("\r?\n|\r", replace_linebreak, x)
  gsub('[ ]{2,}', ' ', y)
}
