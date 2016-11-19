#' Format columns of a data.frame
#'
#' Utility function to apply formating to each column of a data frame.
#' Input can bei either a list of paramters to \link{\code{formatC}},
#' or an arbitrary function to be applied to each column.
#'
#' @param dat a \code{data.frame}
#' @param num_formatC a list of parameters to formatC
#' @param date_formatC a list of parameters to formatC
#' @param dtime_formatC a list of parameters to formatC
#' @param num_fun a function to be applied to all numeric columns
#' @param date_fun a function to be applied to all date columns
#' @param dtime_fun a function to be applied to all POSIXt columns
#' @param col_fun a function to be applied to all columns
#'
#' @return
#' @export
#'
#' @examples
df_format <- function(dat, num_formatC, date_formatC, dtime_formatC,
                      num_fun, date_fun, dtime_fun, col_fun){
  UseMethod('df_format')
}

df_format.default <- function(dat,
                              num_formatC   = NULL,
                              date_formatC  = NULL,
                              dtime_formatC = NULL,
                              num_fun       = identity,
                              date_fun      = identity,
                              dtime_fun     = identity,
                              col_fun       = identity){

  dat <- as.data.frame(dat)

  num_frmt  <- function(.x) do.call(formatC, args = c(list(x = .x),  num_formatC))
  num_frmt  <- purrr::compose(num_frmt, num_fun)

  params      <- list(
    'numeric' = list(fc = num_formatC,   fun = num_fun),
    'Date'    = list(fc = date_formatC,  fun = date_fun),
    'POSIXt'  = list(fc = dtime_formatC, fun = dtime_fun)
  )

  res <- dat

  for(i in c('numeric', 'Date', 'POSIXt')){
    target_vars <- names(dat)[unlist(lapply(dat, is_any_class, i))]
    fc          <- params[[i]]$fc
    ff          <- params[[i]]$fun

    ff %assert_class% 'function'

    if(is.null(fc)){
      formatter <- ff
    } else {
      fc %assert_class% 'list'
      formatter <- function(.x) do.call(formatC, args = c(list(x = .x),  fc))
      formatter <- purrr::compose(formatter, ff)
    }

  res[target_vars] <- lapply(res[target_vars],   formatter)
  }

  res[]           <- lapply(res[], col_fun)

  return(res)
}



