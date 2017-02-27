#' Round columns of a data frame.
#'
#' Rounds all numeric columns in a data.frame
#'
#' @param dat
#' @param digits
#' @seealso round
#'
#' @return
#' @export
#'
#' @examples
df_round <- function(dat, digits = 0){
  assert_that(is.number(digits))
  numcols <- names(dat)[unlist(lapply(dat, is.numeric))]

  for(i in numcols){
    dat[[i]] <- round(dat[[i]], digits = digits)
  }

  return(dat)
}


#' Round columns of a data frame to a specified number of significant digits.
#'
#' Rounds the values in the numeric columns of a data.frame to the specified number of significant digits.
#'
#' @param dat
#' @param digits
#'
#' @return
#' @export
#' @seealso signif
#'
#' @examples
df_signif<- function(dat, digits = 0){
  assert_that(is.number(digits))
  numcols <- names(dat)[unlist(lapply(dat, is.numeric))]

  for(i in numcols){
    dat[[i]] <- signif(dat[[i]], digits = digits)
  }

  return(dat)
}



