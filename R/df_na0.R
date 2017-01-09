#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
df_na0 <- function(dat){
  UseMethod('df_na0')
}

#' @export
df_na0.data.table <- function(dat){
  dat <- data.table::copy(dat)
  for (j in seq_along(dat)){
    set(dat, which(is.nan(dat[[j]]) | is.na(dat[[j]]) ), j, 0)
  }
  return(dat)
}

#' @export
df_na0.data.frame <- function(dat){
  dat[is.na(dat)] <- 0
  return(dat)
}

