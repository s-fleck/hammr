#' Check for equality but treat NAs like normal values
#'
#' @param x Any R object that can be handled by `==`
#' @param y Any R object that can be handled by `==`
#'
#' @return a logical vector.
#'
#' @export
equal_or_na <- function(x, y){
  ((!is.na(x) & !is.na(y)) & (x == y)) | (is.na(x) &  is.na(y))
}





#' Are all values TRUE? Warn if not.
#'
#' Checks if all values of a vector or list are `TRUE`, throws a an warning if
#' not.
#'
#' @param x anything that can be coerced to a list of only `logical`` values
#'   with [as.list()]
#' @param na_value value to replace NAs with (allowed values are `TRUE`,
#'   `FALSE`, `NA`)
#' @param silent `logical`. If `TRUE` no warning is thrown.
#'
#' @return `logical` or `NA`. If `FALSE` or `NA` are returned, a warning will be
#'   thrown containing the names (if `x` is named) or indices of the values that
#'   are not `TRUE`. In addition, the indices of those values will be attached
#'   as an attribute to the returned logical scalar (see examples)
#'
#' @md
#' @seealso [all()]
#' @export
#'
#' @examples
#'
#' x <- list(A = TRUE, B = FALSE)
#' all_with_warning(x)
#'
#' #  [1] FALSE
#' #  attr(,"failed")
#' #  [1] 2
#' #  Warning message:
#' #  In all_with_warning(x) : Not TRUE: B
#'
all_with_warning <- function(
  x,
  na_value = FALSE,
  silent = FALSE
){
  assert_that(is.scalar(na_value))
  assert_that(is.flag(na_value) || is.na(na_value))
  assert_that(is.flag(silent))

  x_lst <- as.list(x)

  assert_that(unique(purrr::map_lgl(x_lst, is.logical)))


  x_lst[is.na(x_lst)] <- na_value
  x_vec <- as.logical(x_lst)
  is_all_true <- all(x_vec)


  if(isTRUE(is_all_true)){
    return(TRUE)

  } else {
    failed_idx   <- which(as.logical(lapply(x_lst, function(x) !isTRUE(x))))
    failed_names <- names(x_lst)[failed_idx]

    # construct warning
    if(!silent){
      if(is.null(failed_names)){
        failed_msg <- failed_idx
      } else {
        failed_msg <- failed_names
      }
      failed_msg <- paste(failed_msg, collapse = ', ')

      warn        <- sprintf('Not TRUE: %s', failed_msg)
      warning(warn)
    }

    attr(is_all_true, 'failed') <- failed_idx
    return(is_all_true)
  }
}




#' Test if all elements of a vector are identical
#'
#' @param x any object that can be handled by [unique()] (usually a vector or
#'   list)
#' @param empty_value Value to return if function is called on a vector of
#'   length 0 (e.g. `NULL`, `numeric()`, ...)
#'
#' @md
#' @family special equality checks
#' @return `TRUE/FALSE`
#' @export
#'
#' @examples
#'
#' all_are_identical(c(1,2,3))
#' all_are_identical(c(1,1,1))
#'
all_are_identical <- function(x, empty_value = FALSE) {
  assert_that(length(empty_value) <= 1)

  # Check inputs
  if(length(x) <= 1L){
    if(identical(length(x), 1L)){
      warning("'x' consists of only one element")
      res <- TRUE
    } else if (identical(length(x), 0L)){
      if(is.null(x)){
        warning("'x' is NULL")
      } else {
        warning("'x' is an empty vector")
      }
      res <- empty_value
    }
  } else {
    res <- identical(length(unique(x)), 1L)
  }

  assert_that(is.flag(res) || identical(res, empty_value))

  return(res)
}




#' Test if all elements of a vector are unique
#'
#' @inheritParams all_are_identical
#' @param silent logical. Suppress Warnings
#'
#' @return TRUE/FALSE
#'
#' @md
#' @family special equality checks
#' @export
#'
#' @examples
#'
#' all_are_identical(c(1,2,3))
#' all_are_identical(c(1,1,1))
#'
all_are_distinct <- function(x, empty_value = FALSE, silent = FALSE){
  assert_that(length(empty_value) <= 1)
  assert_that(is.flag(silent))

  if(length(x) <= 1L){
    if(identical(length(x), 1L)){
      res <- TRUE
      if(!silent) warning("'x' consists of only one element")


    } else if (identical(length(x), 0L)){
      res <- empty_value
      if(!silent){
        if(is.null(x)){
          warning("'x' is NULL")
        } else {
          warning("'x' is an empty vector")
        }
      }
    }


  } else {
    res <- identical(length(unique(x)), length(x))
  }

  assert_that(
    identical(res, TRUE) ||
      identical(res, FALSE) ||
      identical(res, empty_value)
  )

  return(res)
}




#' Unique single element
#'
#' Returns `unique(x)` if all elements of `x`` are identical, throws an error if
#' not.
#'
#' @inheritParams all_are_identical
#'
#' @md
#' @family special equality checks
#' @return A scalar of the same type as `x`
#' @export
unique_single <- function(x){
  res <- unique(x)
  if(is.scalar(res)){
    return(res)
  } else {
    stop('Not all elements of x are identical')
  }
}




#' Read first object of an rda file
#'
#' \url{http://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file}
#'
#' @param infile path to an rda file
#'
#' @return The first object in infile.rda
#' @export
read_rda <- function(infile){
  env <- new.env()
  nm <- load(infile, env)[1]
  if(length(env) > 1){
    warning(sprintf(
      '%s contains more than one object. Returning only the first: %s',
      infile,
      nm
    ))
  }
  env[[nm]]
}



#' Change factor levels according to named character vector
#'
#' This is an alternative interface to [forcats::fct_recode()], that takes
#' a named character vector as input (as opposed to a sequence of length 1
#' character vectors).
#'
#' @param f A factor.
#' @param rec A named character vectors where the name gives the new level, and
#'   the value gives the old level. Levels not otherwise mentioned will be left
#'   as is.
#'
#' @return a factor vector with recoded levels
#' @seealso [forcats::fct_recode()]
#' @export
#'
#' @examples
#'
#' x <- factor(c("apple", "bear", "banana", "dear"))
#' fct_recode2(x, c(fruit = "apple", fruit = "banana"))
#'
#' # [1] fruit bear  fruit dear
#' # Levels: fruit bear dear
#'
fct_recode2 <- function(f, rec){
  assert_that(requireNamespace('forcats'))
  assert_that(is.vector(f) || is.factor(f))
  assert_that(is.vector(rec))
  assert_that(identical(
    length(names(rec)),
    length(rec)
  ))


  args <- vector('list', length(rec))
  for(i in seq_along(args)){
    args[[i]] <- rec[i]
  }
  args <- c(list(as.character(f)), args)

  do.call(forcats::fct_recode, args)
}



#' Get filename (without file extension) from path
#'
#' @param x a character vector of file paths
#'
#' @export
basename_sans_ext <- function(x){
  tools::file_path_sans_ext(basename(x))
}
