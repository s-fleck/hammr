# is_class ----


#' Check if object is of a certain class
#'
#' These functions are desinged to be used in combination with the assertthat
#' pacakge
#'
#' is_class returns TRUE/FALSE. It comes with a on_failure function and
#' is designed to be used in conjunction with the assertthat package.
#' assert_class() and its infix version %assert_class% fail with an error message
#'
#' @param dat any R object
#' @param class the class to be checked for
#'
#' @return is_class returns TRUE/FALSE, assert_class returns TRUE or fails with
#'         an error message.
#' @export
#' @rdname is_class
#'
#' @examples
#'
#' x = data.frame()
#' is.class(x, 'data.frame')
is_class <- function(dat, class){
  class %in% class(dat)
}

on_failure(is_class) <- function(call, env){
  #print(call)
  #class = eval(env$class)
  class = env$class
  paste("Requires an object of class", class, "as input")
}


#' @export
#' @rdname is_class
assert_class <- function(dat, class){
  assert_that(is_class(dat = dat, class = class))
}

#' @export
#' @rdname is_class
`%assert_class%` <- function(dat, class){
  assert_class(dat = dat, class = class)
}

#' @export
#' @rdname is_class
`%is_class%` <- function(dat, class){
  warning('Deprecated. Please user %assert_class% instead.')
  dat %assert_class% class
}


# is_col_classes ----

#' Check for column classes
#'
#' Compares the column classes of a data.frame with
#'
#' @param dat a data.frame or list
#' @param classes a list of column classes. Its names must match
#'        the names of dat exactly (see example)
#' @param method if \code{all}, ensure that all columns named in \code{classes} are present in \code{dat},
#'         if \code{any}, ensure that any of the  columns named in \code{classes} are present in \code{dat},
#'         if \code{identical}, ensure that the names of dat and classes are identical
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


  return(all(res))
}

assertthat::on_failure(is_col_classes) <- function(call, env){
  dat     <- eval(call$dat)
  classes <- eval(call$classes)

  present <- names(classes)[names(classes) %in% names(dat)]
  missing <- names(classes)[!names(classes) %in% names(dat)]
  wrong   <- character()

  for(i in present){
      col    = i
      is     = class(dat[[i]])
      should = classes[[i]]

    if (any(is != should)){
      is_str     <- paste(is, collapse = ', ')
      should_str <- paste(should, collapse = ', ')

      wrong <- paste0(wrong, col, ' (', is_str, '->', should_str, '), ')
    }
  }

  missing <- paste(missing, collapse = ', ')

  msg = character()

  if(length(missing) > 0){
    msg = paste0('Missing from dat: ', missing, '.\n')
  }

  if(length(wrong) > 0){
    wrong <- substr(wrong, 1, nchar(wrong) - 2)
    msg = paste0(msg, 'Wrong classes: ', wrong)
  }

  return(msg)
}


has_field <- function(dat, field, type){
  field_exists           <- field %in% names(dat)
  field_has_correct_type <- ifelse(class(dat[[field]]) == type, TRUE, FALSE)
  return(field_exists && field_has_correct_type)
}


#' Check if any of the classes of the object match a certain string
#'
#' @param dat the object
#' @param choices  the class to be checked for
#'
#' @return True if any of the object classes are the desired class
#' @export
#'
#' @examples
#'
#' x = data.frame()
#' class(x) <- c('data.frame', 'test')
#' is_class(x, 'test')
#'
is_any_class <- function(dat, choices){
  any(choices %in% class(dat))
}

on_failure(is_any_class) <- function(call, env){
  choices = paste(eval(call$choices), collapse=", ")
  paste("Input must be an object of any of the following classes:", choices)
}



