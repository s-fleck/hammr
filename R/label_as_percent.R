#' Label Fraction as Percentage
#'
#' `label_as_percent_bare(x)` is there for covenience when using the function
#' as a ggplot labeler
#'
#' @param x numeric. Will be interpreted as a fraction (e.g. `0.5` will be
#'   interpreted as 50\%)
#' @param format will be passed to [base::sprintf()]
#' @param digits number of digits for rounding
#'
#' @return a character vector.
#' @export
#'
#' @examples
#'
#' label_as_percent(0.5)
#' label_as_percent(0.5, format = "%s [%%]" )
#' label_as_percent_bare(0.5)
#'
label_as_percent <- function(
  x,
  format = "%s %%",
  digits = 0
){
  assert_that(is.numeric(x))
  assert_that(purrr::is_scalar_character(format))
  assert_that(is.number(digits))

  sprintf(
    format,
    round(x * 100, digits = digits)
  )
}




#' @rdname label_as_percent
#' @export
label_as_percent_bare <- function(
  x,
  digits = 0
){
  label_as_percent(x, format = "%s", digits = digits)
}
