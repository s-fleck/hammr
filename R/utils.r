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




#' Weighted Median Value
#'
#' A simple, memory inefficient implementation of weighted median that only
#' supports integer weights. See the \pkg{matrixStats} package for a
#' better implementation
#'
#' @param x a numeric vector
#' @param w a vector of weights, must be the same length as `x`. if `NULL` the
#'   normal median of `x` is returned.
#'
#' @return a numeric or integer scalar
#' @md
#' @export
#'
weighted_median <- function(x, w = NULL){

  if(is.null(w)){
    return(stats::median(x))

  } else {
    assert_that(all(looks_like_integer(w)))
    assert_that(identical(length(x), length(w)))
    w <- as.integer(w)

    return(stats::median(rep(x, w)))
  }
}




#' Title
#'
#' Print each element of `x` in a single line
#'
#' @param x a vector or list of elements that can be handled by [cat()]
#'
#' @return `x` (invisibly)
#' @export
cat_lines <- function(x){
  for(el in x)  cat(el, "\n")
  invisible(x)
}




is_blank <- function(x) trimws(x) == ""
