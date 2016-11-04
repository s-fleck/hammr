#' Title
#'
#' @param dat1 a data.frame
#' @param dat2 a data.frame of the same format as dat1
#' @param fun
#' @param coltypes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
df_compare <- function(dat1, dat2, fun, coltypes, ...){
  UseMethod('df_compare')
}


df_compare.data.table <- function(dat1, dat2, fun, coltypes = 'numeric', ...){

  d1  <- as.data.frame(dat1)
  d2  <- as.data.frame(dat2)
  res <- df_ndiff(d1, d2)

  as.data.table(res)
}


df_compare.data.frame <- function(dat1, dat2, fun, coltypes = 'numeric', digits = NULL, ...){
  assert_that(nrow(dat1)  %identical% nrow(dat2))
  assert_that(ncol(dat1)  %identical% ncol(dat2))
  assert_that(names(dat1) %identical% names(dat2))


  # identify numeric columns
  numcols     <- names(dat1)[lapply(dat1, class) %in% coltypes]
  numcols_chk <- names(dat2)[lapply(dat2, class) %in% coltypes]
  assert_that(numcols %identical% numcols_chk)


  # Apply the function columnwise
  res <- dat1
  for(i in numcols){
    res[[i]] <- fun(dat1[[i]], dat2[[i]], ...)
  }

  if(!is.null(digits)){
    res <- df_round(res, digits = digits)
  }

  return(res)
}


#' numerical difference
df_ndiff <- function(dat1, dat2, coltypes = c('integer', 'numeric'), ...){
  df_compare(dat1, dat2, fun = `-`, coltypes = coltypes, ...)
}


#' proportional difference
df_pdiff <- function(dat1, dat2, coltypes = c('integer', 'numeric'), percent = FALSE, ...){
  df_compare(dat1, dat2, fun = pdiff, coltypes = coltypes, percent = percent, ...)
}


pdiff <- function(x, y, percent){
  res <- (x-y)/(x)
  if(percent) {
    return(res * 100)
  } else{
    return(res)
  }
}



