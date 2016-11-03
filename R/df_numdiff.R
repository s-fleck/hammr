df_numdiff <- function(dat1, dat2, fun){
  UseMethod('df_numdiff')
}


df_numdiff.data.table <- function(dat1, dat2, fun = `-`){

  d1  <- as.data.frame(dat1)
  d2  <- as.data.frame(dat2)
  res <- df_numdiff(d1, d2)

  as.data.table(res)
}


df_numdiff.data.frame <- function(dat1, dat2, fun = `-`, int_as_numeric = TRUE){
  assert_that(nrow(dat1)  %identical% nrow(dat2))
  assert_that(ncol(dat1)  %identical% ncol(dat2))
  assert_that(names(dat1) %identical% names(dat2))


  # convert columns
  if(int_as_numeric){
    dat1 <- typecast_all(dat1, from = 'integer', to = 'numeric')
    dat2 <- typecast_all(dat2, from = 'integer', to = 'numeric')
  }


  # identify numeric columns
    numcols     <- names(dat1)[lapply(dat1, class) == 'numeric']
    numcols_chk <- names(dat2)[lapply(dat2, class) == 'numeric']
    assert_that(numcols %identical% numcols_chk)


  # Apply the function columnwise

  res <- dat1

  for(i in numcols){
    res[[i]] <- fun(dat1[[i]], dat2[[i]])
  }

  return(res)
}
