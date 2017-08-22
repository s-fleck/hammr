#' Round columns of a data frame.
#'
#' `round` rounds the values in its first argument to the specified number of
#' decimal places.
#'
#' @param x a data.frame
#' @inheritParams base::round
#'
#' @return a data.frame
#'
#' @md
#' @family data.frame tools
#' @seealso [round()], [signif()]
#' @export
df_round <- function(x, digits = 0, ...){
  UseMethod('df_round')
}


#' @export
df_round.data.frame <- function(
  x,
  digits = 0,
  ...
){
  assert_that(is.data.frame(x))
  assert_that(is.number(digits))

  numcols <- which(unlist(lapply(x, is.numeric)))

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
df_signif <- function(x, digits = 0){
  assert_that(is.data.frame(x))
  assert_that(is.number(digits))

  numcols <- names(x)[unlist(lapply(x, is.numeric))]

  for(i in numcols){
    x[[i]] <- signif(x[[i]], digits = digits)
  }

  return(x)
}
