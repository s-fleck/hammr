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
write_worksheet <- function(dat, wb, sheet, ...){
  assert_that(requireNamespace("openxlsx"))
  UseMethod('write_worksheet')
}


write_worksheet.Comp_table <- function(
  dat,
  wb,
  sheet,
  ...
){
  wb %assert_class% 'Workbook'

  openxlsx::addWorksheet(wb, sheet)

  titles <- attr(dat, 'titles')
  assert_that(titles %identical% sort(titles))

  title_row <- vector(mode = 'list', length = ncol(dat))
  title_counter <- 1

  for(i in seq_along(title_row)){
    title_row[[i]] <- names(titles)[[title_counter]]

    if(i %in% titles){
      title_counter <- title_counter + 1
    }
  }

  # Write super-headings
  openxlsx::writeData(
    wb,
    sheet = sheet,
    as.data.frame(title_row),
    colNames = FALSE
  )


  for(i in seq_along(titles)){
    merge_start <- ifelse(i == 1L, 1, titles[[i-1]] + 1)
    merge_end   <- titles[[i]]
    openxlsx::mergeCells(
      wb,
      cols = c(merge_start, merge_end),
      rows = 1,
      sheet = sheet
    )
  }

  openxlsx::writeData(
    wb,
    sheet = sheet,
    startRow = 2,
    dat,
    colNames = TRUE
  )

  return(wb)
}
