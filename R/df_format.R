#' Format columns of a data.frame
#'
#' This function applies formatting to each column of a data.frame
#' that matches a specified type. For details refer to the documentation of
#' \link{\code{format}}
#'
#'
#' @param dat a \code{data.frame} (or something that can be coearced with \code{as.data.frame})
#' @param ... Key value pairst of the form classname = list(parameters to \code{format}) (see examples)
#'
#' @return a formatted data.frame
#' @export
#' @rdname df_format
#' @seealso df_format_num
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
#' df_format(tdf,
#'   integer = list(digits = 3, big.mark = ".", decimal.mark = ","),
#'   date    = list("%m/%d/%y"),
#'   POSIXct = list("%m/%d/%y %H:%M:%S"))
#'
#'
df_format <- function(dat, num_format, date_format, dtime_format, col_format){
  UseMethod('df_format')
}


#' Format numeric columns of a data.frame
#'
#' This function applies formatting to each numeric column of a data.frame
#' For details refer to the documentation of \code{\link{format}}
#'
#' @param dat a \code{data.frame} (or something that can be coearced with \code{as.data.frame})
#' @param integer_as_numeric Logical. Should integer columns be formatted the same way as numeric columns?
#' @param ... Params passed on to \code{format}
#'
#' @seealso df_format
#'
#' @export
df_format_num <- function(dat, integer_as_numeric = TRUE, ...){

  if(integer_as_numeric){
    dat <- df_typecast_all(dat, 'integer', 'numeric')
  }

  df_format(dat, numeric = list(...))
}


df_format <- function(dat, ...){

  args <- list(...)
  classes_present <- names(args)[names(args) %in% unlist(lapply(dat, class))] # The unlist is there to deal with columns with multiple classes (such as POSIX columns)

  if(any(!names(args) %in% classes_present)){
    classes_missing <- setdiff(names(args), classes_present)
    warning('Format defined for the following classes that are not present in dat: ', paste(classes_missing, collapse = ', '), '.')
  }

  res <- dat

  for(nm in classes_present){
    format_args <- as.list(args[[nm]])
    formatter   <- function(.x) do.call(format, args = c(list(x = .x),  format_args))

    res <- lapply_by_class(res, formatter, nm)
  }

  return(as.data.frame(res, stringsAsFactors = FALSE))
}
