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
#' @import xtable openxlsx
#'
#' @examples
save_as <- function(dat, outfile, format, ...){


}


#' Title
#'
#' @param dat
#' @param outfile
#' @param format
#' @param header
#' @param footer
#'
#' @return
#' @export
#'
#' @examples
save_as.Rstack_table_xlsx <- function(dat, outfile, sheet_name = 'sheet1', header = NULL, footer = NULL){
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheet_name)

    openxlsx::writeData(wb, sheet_name, header, colNames = FALSE)
    openxlsx::writeData(wb, sheet_name, dat, startRow =nrow(header) +2, colNames = TRUE)
    openxlsx::writeData(wb, sheet_name, footer, startRow = nrow(header) + nrow(dat) + 4, colNames = FALSE)
    openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)
}


#' Title
#'
#' @param dat
#' @param format
#'
#' @return
#' @export
#'
#' @examples
print.Rstack_table_latex <- function(dat){
  alignm <- paste0('XX', paste(rep('X', ncol(dat)-1), collapse = ''))

  print_xtable(dat, alignment = alignm)
}


#' Title
#'
#' @param tab
#' @param digits
#' @param ...
#'
#' @return
#' @export
#' @import xtable
#'
#' @examples
print_xtable <- function(tab, alignment = paste0('ll', paste(rep('X', ncol(tab)-1), collapse = '')), digits = 0, ...) {
  xtable::xtable(tab, align = alignment, digits = digits, ...) %>%
    print(format.args = list(big.mark = "~"),
          include.rownames=FALSE,
          floating = FALSE,
          tabular.environment = 'tabularx',
          booktabs = TRUE,
          sanitize.text.function = identity,
          width = '\\textwidth')
}

