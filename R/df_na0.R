#' Set NA values to Zero (0)
#'
#' Replaces all `NA`s and `NAN`s in a data.frame or
#' data.table with `0`. This will fail if `0` is not a valid
#' value for all columns in `dat` (which can happen, for example, for
#' factor columns)
#'
#' @param dat a data.frame or data.table
#' @param inf logical. if `TRUE`, `inf` values are treated like `NAs`
#'
#' @return A data.frame with all NAs replaced by 0
#'
#' @md
#' @family data.frame tools
#' @export
df_na0 <- function(dat, inf){
  UseMethod('df_na0')
}



#' @export
df_na0.data.table <- function(dat, inf = FALSE){
  assert_that(is.flag(inf))
  dat <- data.table::copy(dat)

  if(!inf){
    selector <- function(x) which(is.na(x) | is.nan(x))
  } else {
    selector <- function(x) which(is.na(x) | is.nan(x) | is.infinite(x))
  }

  for (j in seq_along(dat)) {
    data.table::set(dat, selector(dat[[j]]), j, 0)
  }
  return(dat)
}





#' @export
df_na0.data.frame <- function(dat, inf = FALSE){
  assert_that(is.flag(inf))

  dat[is.na(dat)] <- 0

  if(inf){
    dat[is.infinite(dat)] <- 0
  }

  return(dat)
}
