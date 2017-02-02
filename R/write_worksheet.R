#' Title
#'
#' @param dat
#' @param wb
#' @param sheet
#' @param passed on to \code{openxlsx::writeData}
#'
#' @return
#' @export
#'
#' @examples
write_worksheet <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...
){
  assert_that(requireNamespace("openxlsx"))
  UseMethod('write_worksheet')
}



#' Title
#'
#' @param dat
#' @param wb
#' @param sheet
#' @param append
#' @param start_row
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write_worksheet.default <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...
){
  openxlsx::writeData(wb = wb, sheet = sheet, x = dat, startRow = start_row)
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sanitize_excel_sheet_names <- function(x){
  invalid_chars_regex <- "\\[|\\]|\\*|\\?|:|\\/|\\\\"
  res <- stringi::stri_replace_all_regex(x, invalid_chars_regex,'_')
  stringi::stri_sub(res, 1, 31)
}
