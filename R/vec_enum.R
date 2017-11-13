#' Enumerate Elements of one or multiple vectors
#'
#' @param ... a vector, or several equal length vectors. Vector is here meant
#'   in the broader sense, so it can also be a list of arbitrary R objects.
#'   The [digest] package is used to calculate unique hashes
#'
#' @return an `integer` vector
#' @export
#'
#' @examples
vec_enum <- function(
  ...
){
  x <- list(...)

  assert_that(all(
  purrr::map_lgl(
    x,
    function(.x) {
      rlang::is_vector(x) &&
      identical(length(.x), length(x[[1]]))
    }
  )))

  purrr::map(
    x,
    function(.x) purrr::map_chr(.x, digest::digest)
  ) %>%
    simplify2array() %>%
    apply(1, paste, collapse = "--") %>%
    as.factor() %>%
    as.integer()
}
