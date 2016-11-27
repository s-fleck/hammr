#' Format columns of a data.frame
#'
#' Utility function to apply formating to each column of a data frame.
#' Input can bei either a list of paramters to \code{\link{format}},
#' or an arbitrary function to be applied to each column. If an abriraty
#' function is supplied, all you have to ensure is that it returns a vector
#' of length \code{nrow(dat)}
#'
#' @param dat a \code{data.frame} (or something that can be coearced with \code{as.data.frame})
#' @param num_format a list of parameters to \code{format}, or an arbitrary function
#'        that will be applied to all \code{numeric} columns. You can also supply
#'        an integer (or numeric) vector of length one that will be used as
#'        "digits" arumgent to \code{format} (see examples)
#' @param date_format a list of parameters to \code{format}, or an arbitrary function (\code{\link{format.Date}})
#'        that will be applied to all \code{Date} columns. You can also supply
#'        a character vector of length one that will be used as
#'        "format" arumgent to \code{format} (see examples).
#' @param dtime_format  a list of parameters to \code{format}
#'        (\code{\link{format.POSIXct}}, \code{\link{format.POSIXlt}},
#'        or an arbitrary function)
#'        that will be applied to all \code{POSIXt} columns. You can also supply
#'        a character vector of length one that will be used as
#'        "format" arumgent to \code{format} (see examples).
#' @param col_format a list of parameters to \code{format}, or an arbitrary function
#'        that will be applied to all columns.
#'
#' @return a formatted data.frame
#' @export
#'
#' @examples
#'
#' tdf <- data.frame(
#' a = c("alpha", "beta", "ceta"),
#' b = c(1.230,-1124.0, 1.90),
#' c = c(1L, 3L, 5L),
#' d = as.Date(c("2009-01-12", "2009-01-12", "2009-01-12")),
#' t = as.POSIXct(c("2009-01-12 10:01:01", "2009-01-12 23:01:03", "2009-01-12 16:01:01"), "%Y-%m-%d %H:%M:%S"),
#' stringsAsFactors = FALSE)
#'
#' parenthesise <- function(x) paste0("(", trimws(x) , ")")
#'
#' df_format(tdf,
#'   num_format   = list(digits = 3, big.mark = ".", decimal.mark = ","),
#'   date_format  = list("%m/%d/%y"),
#'   dtime_format = list("%m/%d/%y %H:%M:%S"),
#'   col_format   = parenthesise)
#'
#' If you supply a vector of length one as *_format, df_format will try to guess
#' what you wanted, so
#'
#' df_format(tdf, dtime_format = "%m/%d/%y %H:%M:%S")
#'
#' # is dentical to
#'
#' df_format(tdf, dtime_format = list(format = "%m/%d/%y %H:%M:%S"))
#'
#' # and
#'
#' df_format(tdf, num_format = 2)
#'
#' # is identical to
#'
#' df_format(tdf, num_format = list(digits = 2))
#'
df_format <- function(dat, num_format, date_format, dtime_format, col_format){
  UseMethod('df_format')
}

df_format <- function(dat,
                      num_format   = NULL,
                      int_format   = num_format,
                      date_format  = NULL,
                      dtime_format = NULL,
                      col_format   = NULL){

  dat <- as.data.frame(dat)

  if(is.scalar(num_format) & (
     is.integer(num_format) || is.numeric(num_format))){
    num_format <- list(digits = num_format)
  }

  if(is.scalar(int_format) & (
    is.integer(int_format) || is.numeric(int_format))){
    int_format <- list(digits = int_format)
  }

  if(is.scalar(date_format) && is.character(date_format)) {
    date_format <- list(format = date_format)
  }

  if(is.scalar(dtime_format) && is.character(dtime_format)) {
    dtime_format <- list(format = dtime_format)
  }

  params      <- list(
    'numeric' = num_format,
    'integer' = int_format,
    'Date'    = date_format,
    'POSIXt'  = dtime_format,
    'col'     = col_format
  )

  res <- dat

  for(i in names(params)){
    fc          <- params[[i]]
    target_vars <- names(dat)[unlist(lapply(dat, is_any_class, i))]
    if (i %identical% 'col') target_vars <- names(dat)

    if(is.null(fc) || length(target_vars) %identical% 0L){
      next
    } else if (class(fc) %identical% 'list') {
      formatter <- function(.x) do.call(format, args = c(list(x = .x),  fc))
    } else if (class(fc) %identical% 'function'){
      formatter <- fc
    } else {
      stop(fun_arg_error('*_format parameters must be of class "list" or "function"'))
    }

  res[target_vars] <- lapply(res[target_vars],   formatter)
  }

  assert_that(nrow(dat) %identical% nrow(res))
  assert_that(ncol(dat) %identical% ncol(res))

  return(res)
}



