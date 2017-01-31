#' Save as
#'
#' Generic used by other STAT packages that depend on this package
#'
#' @param dat
#' @param format
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
save_as <- function(dat, outfile, format, ...){
  UseMethod('save_as')
}


#' @export
save_txt <- function(dat, outfile, overwrite = TRUE){
  UseMethod('save_txt')
}

#' As workbook
#'
#' @param dat
#' @param outfile
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
save_xlsx <- function(dat, outfile, overwrite){
  UseMethod('save_xlsx')
}


#' @export
save_xlsx.default <- function(dat, outfile, overwrite = FALSE){
  wb <- as_workbook(dat)
  openxlsx::saveWorkbook(wb, outfile, overwrite)
}
