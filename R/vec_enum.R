#' Enumerate Elements of a Vector or Combination of Vectors
#'
#' @param ... a vector, or several equal length vectors. If `...` is several
#'   vectors, the unique combinations of all elements are indexed.
#'   "Vector" is here meant in the broader sense, so it can also be a list of
#'   arbitrary \R objects.
#'
#'
#' The indexing is based on hashes of each elements of each `...`. See
#'   [digest::digest()] for details on the hashing function.
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
    x, ~ rlang::is_vector(x) && identical(length(.x), length(x[[1]]))
  )))

  purrr::map(x, ~ purrr::map_chr(.x, digest::digest)) %>%
    simplify2array() %>%
    apply(1, paste, collapse = "--") %>%
    forcats::fct_inorder() %>%
    as.integer()
}
