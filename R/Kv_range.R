#' Key_value range
#'
#' @param dat A data frame. Should contain at leas 4 columns: a key column,
#'        a value column, a from and a to column
#' @param from Column of the above data.frame that contains the from values
#' @param to Column of the above data.frame that contains the to values
#'
#' @return
#' @export
#'
#' @examples
kv_range <- function(dat, fromcol = 'from', tocol = 'to'){

  res <- dat %>%
    dplyr::select_(from  = fromcol,
                   to    = tocol,
                   'dplyr::everything()') %>%
    as.data.table(key = c('from', 'to'))

  attr(res, 'date')          <- Sys.time()
  class(res)                 <- union('Kv_range', class(res))

  return(res)
}


#' @export
valid_range <- function(dat, from = 'from', to = 'to'){
  warning('Deprecated. Use kv_range instead.')
  kv_range(dat, from = from, to = to)
}


#' Check if a value is within a key-value range
#'
#' @param key A vector key names
#' @param value A vector of the same lengths with values for the specific key
#' @param kv_range An object of type Valid_range (see \code{\link{valid_range}})
#' @param kv_range_keycol The column in the Valid_range object to compare \code{key} with,
#'
#' @return
#' @export
#'
#' @examples
within_kv_range <- function(key, value, kv_range, kv_range_keycol){
  assert_that(identical(length(key), length(value)))
  assert_that(kv_range_keycol %in% names(kv_range))

  if(any(is.na(kv_range[, c('from', 'to', kv_range_keycol), with = FALSE]))){
    kv_range <- na.omit(kv_range, cols = c('from', 'to', kv_range_keycol))
    warning('Ignoring rows that contain NA values in kv_range in the columns from, to, ', kv_range_keycol)
  }

  dat <- data.table(
    keycol = key,
    value  = value,
    key = c('keycol', 'value')
  ) %>%
    unique()

  cdat <- merge(dat, kv_range, by.x = 'keycol', by.y = kv_range_keycol, all.x = TRUE, all.y = FALSE, sort = FALSE)
  vdat <- cdat[, list(valid = any(!is.na(from) & !is.na(to) & value >= from & value <= to)), keyby = 'keycol']

  res <- vdat$valid[match(key, vdat$keycol)]

  return(res)
}


#' @export
within_valid_range <- function(keycol, value, test_range, check_key){
  warning('Deprecated. please us within_kv_range instead.')
  within_kv_range(keycol, value, kv_range = test_range, kv_range_keycol = check_key)
}

