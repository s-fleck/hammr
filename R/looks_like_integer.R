#' Check if a value looks like integer
#'
#' @param x ...
#' @param na_value What should NAs be treated es (should be `TRUE`, `FALSE` or `NA`)
#' @param scalar allow only scalars (vectors of length `1`)
#'
#' @md
#' @return `TRUE` or `FALSE`
#' @export
looks_like_integer <- function(
  x,
  na_value = FALSE,
  scalar = FALSE
){
  if(scalar && !is.scalar(x)){
    return(FALSE)
  }

  suppressWarnings(
    res <- as.numeric(as.character(x)) %% 1 == 0
  )
  res[is.na(res)] <- FALSE

  res[x == '']    <- na_value
  res[is.na(x)]   <- na_value

  return(res)
}
