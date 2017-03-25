#' Drop columns of a data.frame
#'
#' The main functional difference to other ways to drop columns in R is that it
#' will not throw an error if the columns is not present in the data.frame if
#' allow_partial == TRUE.
#'
#' @param dat a data.frame
#' @param drop a character vector. Names of the columns to be droped
#' @param allow_partial logical. If set to FALSE an error will be thrown
#'   if not all \code{drop} columns are present in \code{dat}.
#'
#' @return A data.frame without the columns specified in drop
#'
#' @family data.frame tools
#' @export
df_drop_cols <- function(dat, drop, allow_partial = TRUE){
  assert_that(is.data.frame(dat))
  assert_that(is.character(drop))
  assert_that(is.flag(allow_partial))

  if(allow_partial){
    drop <- drop[drop %in% names(dat)]

    if(length(drop) > 0){
      message('dropped: ', paste(drop, collapse = ', '))
    } else {
      warning('no columns dropped')
      return(dat)
    }

  } else {
    assert_that(all(drop %in% names(dat)))
  }

  res <- dat %>%
    dplyr::select_(.dots = paste0('-', drop))

  return(res)
}
