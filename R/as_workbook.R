#' As Workbook
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as_workbook <- function(...){
  assert_that(requireNamespace("openxlsx"))
  UseMethod('as_workbook')
}


#' @export
as_workbook.default <- function(dat){
  wb <- openxlsx::createWorkbook()
  wb <- write_worksheet(dat, wb, sheet = 1)
  return(wb)
}
