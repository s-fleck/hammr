stack_table <- function(dat1, dat2, rem_ext = NULL){
  assert_that(nrow(dat1)  %identical% nrow(dat2))
  assert_that(ncol(dat1)  %identical% ncol(dat2))

  dat1 <- as.data.frame(dat1)
  dat2 <- as.data.frame(dat2)

  if(!is.null(rem_ext)){
    names(dat1) <- gsub(rem_ext, '', names(dat1))
    names(dat2) <- gsub(rem_ext, '', names(dat2))
  }

  assert_that(identical(sort(names(dat1)), sort(names(dat2))))
  dat2 <- dat2[names(dat1)]

  res <- list(dat1, dat2)
  class(res) <- c('Stack_table', 'list')
  return(res)
}
