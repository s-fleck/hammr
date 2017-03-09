#' Compare columns of two data frames
#'
#' Combines the columns indicated by \code{coltypes} using the function \code{fun}.
#' The design idea is to use it for comparing two data.frames, but it can be used
#' for other purposes as well, such as adding or multiplying numeric columns of
#' two similar data.frames (see examples)
#'
#' @param dat1 a data.frame
#' @param dat2 a data.frame that has the same number of rows, column names and column types as dat1
#' @param fun a comparison function ()
#' @param coltypes types of the columns of which to apply fun
#' @param ... arguments passed on to fun
#' @rdname df_compare
#'
#' @return a data.frame
#' @export
#'
#' @examples
#'
#' dat1 <- data.frame(
#'   a = c('alpha', 'beta', 'ceta'),
#'   b = c(10,-10, 90),
#'   c = c(1L, 3L, 5L),
#'   d = factor(c('al', 'dl', 'zl')),
#'   stringsAsFactors = FALSE
#' )
#'
#' dat2 <- data.frame(
#'   a = c('alpha', 'beta', 'ceta'),
#'   b = c(10, 20, 100),
#'   c = c(2L, 1L, 0L),
#'   d = factor(c('bl', 'ul', 'dl')),
#'   stringsAsFactors = FALSE
#' )
#'
#' df_ndiff(dat1, dat2)
#' df_pdiff(dat1, dat2)
#'
#' # df_compare can be passed an arbitrary function
#' df_compare(dat1, dat2, `*`)
#' df_compare(dat1, dat2, fun = function(x, y) paste0(x, '/', y))
#' df_compare(dat1, dat2, fun = function(x, y) paste0(x, '/', y), coltypes = 'factor')
df_compare <- function(dat1, dat2, fun, coltypes, ...){
  UseMethod('df_compare')
}




#' @export
df_compare.data.table <- function(dat1, dat2, fun, coltypes = 'numeric', ...){
  d1  <- as.data.frame(dat1)
  d2  <- as.data.frame(dat2)
  res <- df_compare(d1, d2, fun = fun, coltypes = coltypes, ...)

  as.data.table(res)
}




#' @export
df_compare.data.frame <- function(dat1, dat2, fun, coltypes = 'numeric', ...){
  assert_that(nrow(dat1)  %identical% nrow(dat2))
  assert_that(ncol(dat1)  %identical% ncol(dat2))
  assert_that(names(dat1) %identical% names(dat2))

  # identify numeric columns
  numcols     <- which(unlist(lapply(dat1, class) %in% coltypes))
  numcols_chk <- which(unlist(lapply(dat2, class) %in% coltypes))
  assert_that(numcols %identical% numcols_chk)

  # Apply the function columnwise
  res <- dat1
  for(i in numcols){
    res[[i]] <- fun(dat1[[i]], dat2[[i]], ...)
  }

  return(res)
}




#' \code{df_ndiff} subtracts dat2 from dat1 (only integer and numeric columns by default)
#' @rdname df_compare
#' @export
df_ndiff <- function(dat1, dat2, coltypes = c('integer', 'numeric'), ...){
  df_compare(dat1, dat2, fun = `-`, coltypes = coltypes, ...)
}




#' \code{df_pdiff} returns the fractional difference between dat1 and dat2.
#' The formula used is (dat1-dat2)/dat1.
#'
#' @param percent (only df_pdiff) If TRUE, return the difference in percent rather than as a fraction (so 10 instead of 0.1)
#' @rdname df_compare
#' @export
df_pdiff <- function(dat1, dat2, coltypes = c('integer', 'numeric'), percent = FALSE, ...){
  df_compare(dat1, dat2, fun = pdiff, coltypes = coltypes, percent = percent, ...)
}




pdiff <- function(x, y, percent){
  res <- (x-y)/(y)
  if(percent) {
    return(res * 100)
  } else{
    return(res)
  }
}
