#' Valid range
#'
#' @param dat A data frame
#' @param from Column of the above data.frame that contains the from values
#' @param to Column of the above data.frame that contains the to values
#'
#' @return
#' @export
#'
#' @examples
valid_range <- function(dat, from = 'from', to = 'to'){

  res <- dat %>%
    dplyr::select_(from  = from,
                   to    = to,
                   'dplyr::everything()') %>%
    as.data.table(key = c('from', 'to'))

  attr(res, 'date')          <- Sys.time()
  class(res)                 <- union('Valid_range', class(res))

  return(res)
}


#' Check if a value is within a valid range
#'
#' @param key A vector key names
#' @param value A vector of the same lengths with values for the specific key
#' @param test_range An object of type Valid_range (see \code\link{valid_range})
#' @param check_key The column in the Valid_range object to compare \code{key} with,
#'
#' @return
#' @export
#'
#' @examples
within_valid_range <- function(keycol, value, test_range, check_key){
  assert_that(identical(length(keycol), length(value)))
  assert_that(check_key %in% names(test_range))

  if(any(is.na(test_range[, c('from', 'to', check_key), with = FALSE]))){
    test_range <- na.omit(test_range, cols = c('from', 'to', check_key))
    warning('Ignoring rows that contain NA values in test_range in the columns from, to, ', check_key)
  }

  dat <- data.table(
    keycol = keycol,
    value  = value,
    key = c('keycol', 'value')
  ) %>%
    unique()

  cdat <- merge(dat, test_range, by.x = 'keycol', by.y = check_key, all.x = TRUE, all.y = FALSE, sort = FALSE)
  vdat <- cdat[, list(valid = any(!is.na(from) & !is.na(to) & value >= from & value <= to)), keyby = 'keycol']

  res <- vdat$valid[match(keycol, vdat$keycol)]

  return(res)
}



