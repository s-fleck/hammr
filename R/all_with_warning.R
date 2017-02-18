#' @export
all_with_warning <- function(dat){
  dat <- as.list(dat)
  dat %assert_class% 'list'
  assert_that(unlist(unique(lapply(dat, class))) %identical% 'logical')

  datl <- as.logical(dat)
  if(any(is.na(datl))){
    warning("Treating NAs as 'FALSE'")
  }

  dat[which(is.na(datl))] <- FALSE
  datl[is.na(datl)]       <- FALSE

  if(all(datl)){
    return(TRUE)
  } else {
    failed      <- dat[as.logical(lapply(dat, identical, FALSE))]
    warn        <- paste(
      'FALSE, but should be TRUE:\n',
      paste(names(failed), collapse = ', ')
    )
    warning(warn)
    return(FALSE)
  }
}
