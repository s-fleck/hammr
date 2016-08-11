#' Check if object is of a certain class
#'
#' @param dat the object
#' @param class the class to be checked for
#'
#' @return True if object is of the desired class
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
`%assert_class%` <- function(dat, class){
  assert_that(is_class(dat = dat, class = class))
}

#' @export
#' @rdname is_class
`%is_class%` <- function(dat, class){
  warning('Deprecated. Please user %assert_class% instead.')
  dat %assert_class% class
}

#' Check for column classes
#'
#' @param dat a data.frame
#' @param classes a list of column classes, like list(colname = 'character', colname2 = 'numeric')
#' @export
is_col_classes <- function(dat, classes = list()){

  res <- vector()

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


#' Check if a value looks like integer
#'
#' @param x ...
#' @param na_value What should NAs be treated es (should be TRUE, FALSE or NA)
#'
#' @return TRUE / FALSE
#' @export
looks_like_integer <- function(x, na_value = FALSE){

  suppressWarnings(
    res <- as.numeric(as.character(x)) %% 1 == 0
  )

  res[x == '']    <- na_value
  res[is.na(x)]   <- na_value
  res[is.na(res)] <- FALSE

  return(res)
}


#' Check if an object is valid
#'
#' requires that is_valid.class is defined somewhere
#'
#' @param x an R object
#'
#' @return logical; whether this object meets pre-defined validity conditions.
#' @export
is_valid <- function(x) {
  UseMethod("is_valid")
}

#' @export
on_failure(is_valid) <- function(call, env){
  cls <- class(deparse(call$x))
  cls <- paste(cls, collapse = ', ')
  paste("A validity check failed for object of class", cls)
}

