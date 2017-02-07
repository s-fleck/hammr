#' Set NA values to Zero (0)
#'
#' Replaces all \code{NA}s and \code{NAN}s in a \code{data.frame} or
#' \code{data.table} with \code{0}. This will fail if \code{0} is not a valid
#' value for all columns in \code{dat} (which can happen, for example, for
#' factor columns)
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
  for (j in seq_along(dat)) {
    data.table::set(
      dat,
      which(is.nan(dat[[j]]) |
            is.na(dat[[j]])  ),
      j,
      0)
  }
  return(dat)
}

#' @export
df_na0.data.frame <- function(dat){
  dat[is.na(dat)] <- 0
  return(dat)
}
