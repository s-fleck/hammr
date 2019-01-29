#' Round columns of a data frame.
#'
#' `round` rounds the values in its first argument to the specified number of
#' decimal places.
#'
#' @param x a data.frame
#' @param ignore `character` vector. columns of `x` that should not be rounded
#' @inheritParams base::round
#'
#' @return a data.frame
#'
#' @md
#' @family data.frame tools
#' @seealso [round()], [signif()]
#' @export
df_round <- function(x, digits = 0, ignore = NULL, ...){
  UseMethod('df_round')
}


#' @export
df_round.data.frame <- function(
  x,
  digits = 0,
  ignore = NULL,
  ...
){
  assert_that(is.data.frame(x))
  assert_that(is.number(digits))
  assert(is.null(ignore) || all(ignore %in% names(x)))

  numcols <- vapply(x, is.numeric, logical(1))
  numcols <- numcols & !names(x) %in% ignore
  numcols <- which(numcols)

  for(i in numcols){
    x[[i]] <- round(x[[i]], digits = digits)
  }

  return(x)
}




#' `signif` rounds the values in the numeric columns of a data.frame to the
#' specified number of significant digits.
#'
#' @md
#' @rdname df_round
#' @export
df_signif <- function(x, digits = 0, ignore = NULL){
  assert_that(is.data.frame(x))
  assert_that(is.number(digits))
  assert(is.null(ignore) || all(ignore %in% names(x)))

  numcols <- vapply(x, is.numeric, logical(1))
  numcols <- numcols & !names(x) %in% ignore
  numcols <- which(numcols)

  if (length(ignore)){
    assert(is.character(ignore))
    assert(all(ignore %in% names(x)))
    numcols <- setdiff(numcols, ignore)
  }

  for(nm in numcols){
    x[[nm]] <- signif(x[[nm]], digits = digits)
  }

  return(x)
}
