#' Drop a column from a data.frame if it exists
#'
#' @param dat data.frame
#' @param drop character vector of the columns that are to be dropped
#'
#' @return a data.frame without the columns specified in drop
#' @export
drop_if_exists <- function(dat, drop){
  dat %assert_class% 'data.frame'

  drop <- paste0('^', drop, '$')

  rex <- paste0(drop, collapse = '|')
  #rex <- paste0('[', rex, ']')
  sel <- !grepl(rex, names(dat))

  dropped_names <- names(dat)[!sel]

  if(length(dropped_names) > 0){
    message('dropped: ', dropped_names, collapse = ', ')
  }

  res <- dat[sel]
  return(res)
}
