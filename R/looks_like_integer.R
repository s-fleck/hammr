#' Check if a value looks like integer
#'
#' `looks_like_integer()` checks if a value is a whole number, even if it is
#' supplied as a character or factor value.
#'
#' @param x any R object
#' @param na_value What value should be returned for NAs (sensible values are `TRUE`,
#' `FALSE` or `NA`)
#'
#' @md
#' @return `looks_like_integer()` returns a logical vector,
#'   `looks_like_scalar_integer()` always returns a single logical value.
#' @export
#'
#' @examples
#' # evaluates to TRUE
#' looks_like_integer(1)
#' looks_like_integer('1')
#' looks_like_integer('1.0')
#'
#' # evaluates to FALSE
#'
#' looks_like_integer(1.1)
#' looks_like_integer('1.1')
looks_like_integer <- function(
  x,
  na_value = FALSE
){
  suppressWarnings(
    res <- as.numeric(as.character(x)) %% 1 == 0
  )

  res[is.na(res)] <- FALSE
  res[is.na(x)]   <- na_value

  return(res)
}




#' @rdname looks_like_integer
looks_like_scalar_integer <- function(x, na_value = FALSE){
  if(is.scalar(x)){
    return(looks_like_integer(x, na_value))
  } else {
    return(FALSE)
  }
}
