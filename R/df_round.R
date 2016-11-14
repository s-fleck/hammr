#' Title
#'
#' @param dat
#' @param digits
#'
#' @return
#' @export
#'
#' @examples
df_round <- function(dat, digits = 0){
  numcols <- names(dat)[lapply(dat, class) == 'numeric']

  for(i in numcols){
    dat[[i]] <- round(dat[[i]], digits = digits)
  }

  return(dat)
}
