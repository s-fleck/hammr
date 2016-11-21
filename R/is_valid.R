#' Check if an object is valid
#'
#' requires that is_valid.class is defined somewhere
#'
#' @param x an R object
#'
#' @return logical; whether this object meets pre-defined validity conditions.
#' @export
is_valid <- function(x, ...) {
  UseMethod("is_valid")
}

#' @export
on_failure(is_valid) <- function(call, env){
  cls <- class(deparse(call$x))
  cls <- paste(cls, collapse = ', ')
  paste("A validity check failed for object of class", cls)
}


warn_invalid <- function(dat){
  dat %assert_class% 'list'
  assert_that(unlist(unique(lapply(dat, class))) %identical% 'logical')

  if(all(as.logical(dat))){
    return(TRUE)
  } else {
    failed      <- dat[as.logical(lapply(dat, identical, FALSE))]
    warn        <- 'A validity check has vailed for:' %_% paste(names(failed), collapse = ', ')
    warning(warn)
    return(FALSE)
  }
}


