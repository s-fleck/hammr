#' Title
#'
#' @param dat
#' @param num_format
#' @param date_format
#' @param format_fun
#'
#' @return
#' @export
#'
#' @examples
df_format <- function(dat, num_format, date_format, format_fun) {UseMethod('df_format')}

df_format.default <- function(dat, num_format = list(digits = 1, format = 'f'), date_format = NULL, format_fun = identity){
  dat <- as.data.frame(dat)

  num_frmt      <- function(.x) do.call(formatC, args = c(list(x = .x),  num_format))
  num_vars      <- names(dat)[unlist(lapply(dat, class)) == 'numeric']
  dat[num_vars] <- lapply(dat[num_vars], num_frmt)

  if(!is.null(date_format)){
    stop('not yet implemented')
  }

  dat           <- purrr::map_df(dat, format_fun)

  return(dat)
}


df_format.data.table <- function(dat, num_format = list(digits = 1, format = 'f'), date_format = NULL, format_fun = identity){
  dat <- data.table::copy(dat)

  num_frmt      <- function(.x) do.call(formatC, args = c(list(x = .x),  num_format))
  num_vars      <- names(dat)[unlist(lapply(dat, class)) == 'numeric']

  dat[, (num_vars) := lapply(.SD, num_frmt), .SDcols = num_vars]

  if(!is.null(date_format)){
    stop('not yet implemented')
  }

  dat[,] <- lapply(dat[,], format_fun)

  return(dat)
}
