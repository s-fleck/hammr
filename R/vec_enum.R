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
#'
#' vec_enum(c("a", "a", "b", "a"))
#' vec_enum(list(iris, cars, iris, cars, cars))
#'
#' x <- data.frame(
#'   color1 = c("blue", "red", "blue", "green", "red"),
#'   color2 = c("dog", "cat", "dog", "cat", "dog")
#' )
#'
#' vec_enum(x$color1, x$color2)
#'
vec_enum <- function(
  ...
){
  assert_namespace("digest")
  assert_namespace("forcats")
  x <- list(...)

  assert_that(all(
    vapply(
      x, function(.x) { identical(length(.x), length(x[[1]])) }, logical(1)
  )))

  lapply(x, function(.x) vapply(.x, digest::digest, character(1))) %>%
    simplify2array() %>%
    apply(1, paste, collapse = "--") %>%
    forcats::fct_inorder() %>%
    as.integer()
}
