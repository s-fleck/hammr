#' Round columns of a data frame.
#'
#' `round` rounds the values in its first argument to the specified number of
#' decimal places.
#'
#' @param dat a data.frame
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
  dat,
  digits = 0,
  ...
){
  assert_that(is.data.frame(dat))
  assert_that(is.number(digits))

  numcols <- which(unlist(lapply(dat, is.numeric)))

  for(i in numcols){
    dat[[i]] <- round(dat[[i]], digits = digits)
  }

  return(dat)
}




#' `signif` rounds the values in the numeric columns of a data.frame to the
#' specified number of significant digits.
#'
#' @md
#' @rdname df_round
#' @export
df_signif <- function(dat, digits = 0){
  assert_that(is.data.frame(dat))
  assert_that(is.number(digits))

  numcols <- names(dat)[unlist(lapply(dat, is.numeric))]

  for(i in numcols){
    dat[[i]] <- signif(dat[[i]], digits = digits)
  }

  return(dat)
}
