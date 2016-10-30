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
