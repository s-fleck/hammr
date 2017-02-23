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
  dplyr::mutate_if(dat, is.numeric, round, digits = digits)
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
  dplyr::mutate_if(dat, is.numeric, signif, digits = digits)
}



