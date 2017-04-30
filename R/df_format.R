#' Format columns of a data.frame
#'
#' \tabular{ll}{
#'   `df_format` \tab applies [format()] to each column of a
#'                    data.frame that matches a specific class\cr
#'   `df_formatC` \tab applies [formatC()] instead. \cr
#' }
#'
#'
#' @param dat a data.frame, or something that can be coerced to one with
#'   [as.data.frame()]
#' @param ... Key value pairst of the form `classname = list(argument = value)`
#'   (see examples)
#'
#' @return a formatted data.frame
#'
#' @family data.frame tools
#' @seealso [format()], [format.Date()], [format.POSIXct()], [formatC()]
#' @rdname df_format
#'
#' @md
#' @export
#'
#' @examples
#'
#' tdf <- data.frame(
#'   a = c("alpha", "beta", "ceta"),
#'   b = c(1.230,-1124.0, 1.90),
#'   c = c(1L, 3L, 5L),
#'   d = as.Date(c("2009-01-12", "2009-01-12", "2009-01-12")),
#'   t = as.POSIXct(
#'     c("2009-01-12 10:01:01", "2009-01-12 23:01:03", "2009-01-12 16:01:01"),
#'     "%Y-%m-%d %H:%M:%S"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' df_format(tdf,
#'   integer = list(digits = 3, big.mark = ".", decimal.mark = ","),
#'   date    = list("%m/%d/%y"),
#'   POSIXct = list("%m/%d/%y %H:%M:%S")
#' )
#'
df_format <- function(dat, ...){
  df_format_internal(dat, ..., base_formatter = format)
}



#' @rdname df_format
#' @export
df_formatC <- function(dat, ...){
  df_format_internal(dat, ..., base_formatter = formatC)
}




df_format_internal <- function(dat, ..., base_formatter = format){
  args <- list(...)

  # The unlist is there to deal with columns with multiple classes
  # (such as POSIX columns)
  classes_present <- names(args)[names(args) %in% unlist(lapply(dat, class))]

  if(any(!names(args) %in% classes_present)){
    classes_missing <- setdiff(names(args), classes_present)
    warning(
      'Format defined for the following classes that are not present in dat: ',
      paste(classes_missing, collapse = ', '), '.'
    )
  }

  res <- dat

  for(nm in classes_present){
    format_args <- as.list(args[[nm]])
    formatter   <- function(.x){
      do.call(base_formatter, args = c(list(x = .x),  format_args))
    }

    res <- lapply_if_class(res, formatter, nm)
  }

  return(as.data.frame(res, stringsAsFactors = FALSE))
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
#' @family data.frame tools
#'
#' @export
df_format_num <- function(dat, integer_as_numeric = TRUE, ...){

  if(integer_as_numeric){
    dat <- df_typecast_all(dat, 'integer', 'numeric')
  }

  df_format(dat, numeric = list(...))
}
