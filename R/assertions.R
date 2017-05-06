#' Assert that an object has a specifc class
#'
#' @param dat any R object
#' @param class the class to be checked for
#'
#' @return either `TRUE` or raises an error
#' @export
#' @seealso [assertthat::assert_that()]
#' @md
#' @rdname assert_class
#' @examples
#'
#' x = data.frame()
#' assert_class(x, 'data.frame')
#'
#' @export
#' @rdname assert_class
assert_class <- function(dat, class){
  assert_that(inherits(dat, class))
}


#' @export
#' @rdname assert_class
`%assert_class%` <- function(dat, class){
  assert_class(dat = dat, class = class)
}




#' Check if the columns of a data.frame
#'
#' Check if the columns of a data.frame are of predefined classes.
#'
#' @param dat a data.frame or list
#' @param classes a named list of column classes. Its names must match the names
#'   of dat
#' @param method if \code{all}, ensure that all columns named in \code{classes}
#'   are present in \code{dat}, if \code{any}, ensure that any of the  columns
#'   named in \code{classes} are present in \code{dat}, if \code{identical},
#'   ensure that the names of dat and classes are identical (and in the same
#'   order)
#' @export
is_col_classes <- function(dat, classes, method = 'identical'){
  classes %assert_class% 'list'
  assert_that(length(classes) > 0)
  assert_that(length(names(classes)) %identical% length(classes))
  assert_that(is.scalar(method))
  assert_that(method %in% c('all', 'any', 'identical'))

  dat <- as.list(dat)

  if(method %identical% 'all'){
    assert_that(all(names(classes) %in% names(dat)))
  } else if (method %identical% 'any'){
    assert_that(any(names(classes) %in% names(dat)))
    classes <- classes[names(classes) %in% names(dat)]
  } else if (method %identical% 'identical'){
    assert_that(identical(names(classes), names(dat)))
  } else{
    stop('method must be "all", "any" or "identical"')
  }

  res <- rep(FALSE, length(names(classes)))
  names(res) <- names(classes)

  for(i in names(classes)){
    res[i] <- i %in% names(dat) && classes[[i]] == class(dat[[i]])
  }

  all_with_warning(res)
}




assertthat::on_failure(is_col_classes) <- function(call, env){
  dat     <- eval(call$dat)
  classes <- eval(call$classes)

  present <- names(classes)[names(classes) %in% names(dat)]
  missing <- names(classes)[!names(classes) %in% names(dat)]
  wrong   <- character()

  for(i in present){
      col    <- i
      is     <- class(dat[[i]])
      should <- classes[[i]]

    if (any(is != should)){
      is_str     <- paste(is, collapse = ', ')
      should_str <- paste(should, collapse = ', ')

      wrong <- paste0(wrong, col, ' (', is_str, '->', should_str, '), ')
    }
  }

  missing <- paste(missing, collapse = ', ')

  msg <- character()

  if(length(missing) > 0){
    msg <- paste0('Missing from dat: ', missing, '.\n')
  }

  if(length(wrong) > 0){
    wrong <- substr(wrong, 1, nchar(wrong) - 2)
    msg <- paste0(msg, 'Wrong classes: ', wrong)
  }

  return(msg)
}
