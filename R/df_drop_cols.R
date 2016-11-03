#' Drop columns of a data.frame
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

#' Drop a column from a data.frame if it exists
#'
#' @param dat data.frame

#'
#' @return a data.frame without the columns specified in drop
#' @export
drop_if_exists <- function(dat, drop){
  warning('Deprecated. Please us df_drop_cols')
  df_drop_columns(dat, drop, allow_partial = TRUE)
}
