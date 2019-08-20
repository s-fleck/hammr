#' Determine Duplicated Combinations of Elements
#' 
#' Determines which combinations of elements are duplicates of elements with 
#' smaller subscripts. The difference to [duplicated()] is that 
#' `duplicated_combinations()` counts c("A", "C") as a duplicate of c("C", "A")
#' while `duplicated()` does not.
#' 
#' `duplicated_combinations()` is just a wrapper around [`base::duplicated()`], 
#'  so for implementation details please refer to that functions documentation.
#'  Please be aware that all elements are coerced to `character` before 
#'  comparison, with all the associated limitations. 
#'  
#' @param x a `list` of vectors of equal length, a `matrix` or a `data.frame`.
#' @inheritParams base::duplicated
#'
#' @return A logical vector of the same length as the elements to be compared,
#'   e.g. `nrow(x)` if `x` is a `data.frame` or `matrix`.
#'   
#' @export
#'
#' @examples
#' x <- data.frame(
#'   a = factor(c("A", "B", "C", "E", "G")),
#'   b = factor(c("C", "B", "A", "E", "F")),
#'   c = 1:5,
#'   stringsAsFactors = FALSE
#' ) 
#' 
#' duplicated_combinations(x[, 1:2])
#' 
duplicated_combinations <- function(
  x,
  incomparables = FALSE,
  fromLast = FALSE  #nolint
){
  UseMethod("duplicated_combinations")
}




#' @rdname duplicated_combinations
#' @export
duplicated_combinations.data.frame <- function(
  x,
  incomparables = FALSE,
  fromLast = FALSE  #nolint
){
  duplicated_combinations(
    as.matrix(x),
    incomparables = incomparables,
    fromLast = fromLast  #nolint
  )
}




#' @rdname duplicated_combinations
#' @export
duplicated_combinations.list <- function(
  x,
  incomparables = FALSE,
  fromLast = FALSE  #nolint
){
  assert_that(is_equal_length(x))
  assert_that(all(
    vapply(x, is.atomic, logical(1) )
  ))

  dat <- matrix(unlist(x), ncol = length(x) )
  duplicated_combinations(
    dat,
    incomparables = incomparables,
    fromLast = fromLast   #nolint
  )
}




#' @rdname duplicated_combinations
#' @export
duplicated_combinations.matrix <- function(
  x,
  incomparables = FALSE,
  fromLast = FALSE  #nolint
){
  duplicated(
    apply(x, 1, function(y) paste(sort(y), collapse = "-.-")),
    incomperables = incomparables,
    fromLast = fromLast  #nolint
  )
}
