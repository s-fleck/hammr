#' Drop columns of a data.frame
#'
#' Drops columns of a data.frame if they exists. The main functional difference
#' to other ways to drop columns in R is that it will not throw an error of
#' the columns is not present in the data.frame.
#'
#' @param dat a data.frame
#' @param drop (character vector) names of the columns to be droped
#' @param allow_partial (logical vector) If set to FALSE an error will be thrown
#'        if not all \code{drop} columns are present in \code{dat}
#'
#' @return
#' @export
#'
#' @examples
df_drop_cols <- function(dat, drop, allow_partial = TRUE){
  dat %assert_class% 'data.frame'
  allow_partial %assert_class% 'logical'
  assert_that(is.scalar(allow_partial))

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
