#' Title
#'
#' @param dat
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print_tex <- function(dat, ...){
  UseMethod('print_tex')
}


#' print a Stack_table as latex
#'
#' Stacked rows are sepparated by \code{\\newline}, therefor this only works
#' correctly for columns that have an 'X' column type (see documentation of
#' the tabularx latex package). If you want each stack element in a proper
#' tabular row, use \code{xtable::xtable(as.data.frame(dat))} instead.
#' By default this uses the \code{tabularx} and \code{booktabs} latex packages.
#' Booktabs can be switched of, but the tabularx package is hardcoded into the
#' xtable settings right now as some of the formatting magic depends on it
#'
#' @param dat an input stack table
#' @param stack Stack by by \code{row} or \code{col}
#' @param insert_blank_row insert additional empty row after each row of the
#'        tabular environment. \renewcommand{\arraystretch}{1.5}
#' @param .align the [pos] argument of the tabular environment. Passed on
#'        to \code{\link{xtable}}. Please note that \code{\\newline} only
#'        works in \code{X} columns. If you use another format the 'stacking'
#'        will not work correclty.
#' @param .caption passed on to \code{\link{xtable}}
#' @param .include.rownames passed on to \code{\link{print.xtable}}
#' @param .floating passed on to \code{\link{print.xtable}}
#' @param .tabular.environment passed on to \code{\link{print.xtable}}
#' @param .booktabs passed on to \code{\link{print.xtable}}
#' @param .sanitize.text.function passed on to \code{\link{print.xtable}}
#' @param .width passed on to \code{\link{print.xtable}}
#' @param ... Additoinal arguments passed on to \code{\link{print.xtable}}
#'
#' @return
#' @export
#'
#' @examples
print_tex.Stack_table <- function(dat,
                                  stack_method = 'row',
                                  insert_blank_row = (stack_method == 'row'),
                                  .align = paste0('lX',
                                                  paste(rep('X',
                                                            ncol(dat[[1]]) - 1),
                                                        collapse = '')),
                                  .include.rownames=FALSE,
                                  .floating = FALSE,
                                  .booktabs = TRUE,
                                  .sanitize.text.function = identity,
                                  .width = '\\textwidth',
                                  .caption = NULL,
                                  ...){

  # Preconditions
  stack_method %assert_class% 'character'
  insert_blank_row %assert_class% 'logical'
  assert_that(is.scalar(stack_method))
  assert_that(is.scalar(insert_blank_row))


  # Stacking
  res <- switch(stack_method,
                'row' = stack_rows_tex(dat, insert_blank_row = FALSE),
                'col' = stack_cols_tex(dat))

  # format latex
  xtable::xtable(
    res,
    align = .align,
    caption = .caption
  ) %>%
    print(
      include.rownames = .include.rownames,
      tabular.environment = 'tabularx',
      booktabs = .booktabs,
      sanitize.text.function = .sanitize.text.function,
      width = .width,
      ...)
}




