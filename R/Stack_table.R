stack_table <- function(dat1, dat2, rem_ext = NULL){
  dat1 %assert_class% 'data.frame'
  dat2 %assert_class% 'data.frame'
  assert_that(nrow(dat1)  %identical% nrow(dat2))
  assert_that(ncol(dat1)  %identical% ncol(dat2))

  dat1 <- as.data.table(copy(dat1))
  dat2 <- as.data.table(copy(dat2))

  if(!is.null(rem_ext)){
    setnames(dat1, gsub(rem_ext, '', names(dat1)))
    setnames(dat2, gsub(rem_ext, '', names(dat2)))
  }

  assert_that(identical(sort(names(dat1)), sort(names(dat2))))
  dat2 <- dplyr::select_(dat2, .dots = names(dat1))

  res <- list(dat1, dat2)
  class(res) <- c('Stack_table', 'list')
  return(res)
}

#' print a Stack_table as latex
#'
#' Stacked rows are sepparated by \code{\newline}, therefor this only works
#' correctly for columns that have an 'X' column type (see documentation of
#' the tabularx latex package). If you want each stack element in a proper
#' tabular row, use \code{xtable::xtable(as.data.frame(dat))} instead.
#' By default this uses the \code{tabularx} and \code{booktabs} latex packages.
#' Booktabs can be switched of, but the tabularx package is hardcoded into the
#' xtable settings right now as some of the formatting magic depends on it
#'
#' @param dat an input stack table
#' @param stack Stack by by \code{row} or \code{col}
#' @param insert_empty_row insert additional empty row after each row of the
#'        tabular environment. \renewcommand{\arraystretch}{1.5}
#' @param .align the [pos] argument of the tabular environment. Passed on
#'        to \code{\link{xtable}}. Please note that \code{\newline} only
#'        works in \code{X} columns. If you use another format the 'stacking'
#'        will not work correclty.
#' @param .include.rownames passed on to \code{\link{print.xtable}}
#' @param .floating passed on to \code{\link{print.xtable}}
#' @param .tabular.environment passed on to \code{\link{print.xtable}}
#' @param .booktabs passed on to \code{\link{print.xtable}}
#' @param .sanitize.text.function passed on to \code{\link{print.xtable}}
#' @param .width passed on to \code{\link{print.xtable}}
#' @param ... Additoinal arguments passed on to \code{\link{xtable}} and
#'        \code{\link{print.xtable}}
#'
#' @return
#' @import xtable data.table foreach
#' @export
#'
#' @examples
print_tex.Stack_table <- function(dat,
                                  stack_method = 'row',
                                  insert_empty_row = (stack_method == 'row'),
                                  .align = paste0('lX', paste(rep('X', ncol(dat[[1]])-1), collapse = '')),
                                  .include.rownames=FALSE,
                                  .floating = FALSE,
                                  .booktabs = TRUE,
                                  .sanitize.text.function = identity,
                                  .width = '\\textwidth',
                                  ...){

  # Preconditions
    stack_method %assert_class% 'character'
    insert_empty_row %assert_class% 'logical'
    assert_that(is.scalar(stack_method))
    assert_that(is.scalar(insert_empty_row))


  # Stacking
  res <- switch(stack_method,
         'row' = stack_rows_tex(dat, insert_empty_row = FALSE),
         'col' = stack_cols_tex(dat))

  # format latex
    xtable::xtable(res, align = .align, ...) %>%
      print(include.rownames = .include.rownames,
            floating = .floating,
            tabular.environment = 'tabularx',
            booktabs = .booktabs,
            sanitize.text.function = .sanitize.text.function,
            width = .width,
            ...)
}



save_as.Stack_table <- function(dat, outfile, format){

}


#' Title
#'
#' @param dat
#' @param stack_method
#' @param ... passed on to as.data.frame.data.table
#'
#' @return
#' @export
#'
#' @examples
as.data.table.Stack_table <- function(dat, stack_method = 'row'){
  stack_method  %assert_class% 'character'
  assert_that(is.scalar(stack))
  if(stack_method %in% c('c', 'col', 'column', 'columns')){
    res <- stack_cols(dat)
  } else if(stack_method %in% c('r', 'row', 'rows')) {
    res <- stack_rows(dat)
  } else{
    stop('stack_method must be either "row" or "col".')
  }

  return(as.data.table(res))
}


#' Title
#'
#' Stacking uses \code{data.table}s internally,
#'
#' @param dat
#' @param stack_method
#' @param ... parameters passed on to \code{as.data.frame.data.table}
#'
#' @return
#' @export
#'
#' @examples
as.data.frame.Stack_table <- function(dat, stack_method = 'row'){
  as.data.frame(as.data.table(dat))
}

# Utility funs -----------------------------------------------------------------
stack_rows <- function(dat){
  dat %assert_class% 'Stack_table'
  res <- rbind(dat[[1]], dat[[2]])

  roworder <- foreach(i = seq_len(nrow(dat[[1]])), .combine = c) %do% {
    c(i, i + nrow(dat[[1]]))
  }

  assert_that(max(roworder) %identical% nrow(res))
  return(res[roworder])
}


stack_rows_tex <- function(dat, insert_empty_row) {
  dat %assert_class% 'Stack_table'

  empty_row <- rep('', length(dat[[1]])) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    data.table::setnames(names(dat[[2]]))

  res <- foreach(i = 1:nrow(dat[[1]]), .combine = rbind) %do% {
    r <- paste(dat[[1]][i,], dat[[2]][i,], sep = ' \\newline ') %>%
      t() %>%
      as.data.frame()
    names(r) <-  names(dat[[1]])


    if (insert_empty_row && i != nrow(dat[[1]])) {
      r <- rbind(r, empty_row)
    }

    return(r)
  }
}


stack_cols <- function(dat){
  dat %assert_class% 'Stack_table'
  res <- cbind(dat[[1]], dat[[2]])

  colorder <- foreach(i = seq_along(dat[[1]]), .combine = c) %do% {
    c(i, i+ncol(dat[[1]]))
  }

  assert_that(max(colorder) %identical% ncol(res))
  setcolorder(res, colorder)
  return(res)
}


stack_cols_tex <- function(dat, insert_empty_row) {

  res <- foreach(i = 1:nrow(dat[[1]]), .combine = rbind) %do% {
    r <- paste(dat[[1]][i,], dat[[2]][i,], sep = ' ') %>%
      t() %>%
      as.data.frame()
    names(r) <-  names(dat[[1]])

    return(r)
  }
}


