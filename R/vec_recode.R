#' Recode vector
#'
#' Recode a vector based on two mapping vectors. This is just a wrapper
#' around [base::match()] that implements some sanity checks for its inputs.
#' `factors` are not supported as `x` and `from` arguments it is not clear
#' wether they should be treated as `integer` or `character` please coerce
#' manually.
#'
#' @param x any atomic vector except `factors`
#' @param from any atomic vector except `factors`. All values of `from` must be
#'   unique, and all values of `x` must be in `from`.
#' @param to a vector. must be the same length as `from` (can be a `factor`)
#'
#'
#' @return a vector of the same type as `to`
#'
vec_recode <- function(
  x,
  from,
  to
){
  assert_that(
    length(from) > 0L,
    identical(length(from), length(to)),
    is.atomic(from) && is.atomic(to),
    all_are_distinct(from),
    all(x %in% from)
  )

  assert(
    !is.factor(x) && !is.factor(from),
    "vec_recode does not support factors in its `x` and `from` argument."
  )

  res <- to[match(x, from)]

  # postconditions
  assert(
    identical(length(x), length(res))
  )

  res
}
