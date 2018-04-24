#' Split a Data Frame Into a List of Data Frames at Regular Intervals
#'
#' @param x a `data.frame`
#' @param interval `integer` scalar. Number of rows per list element
#'
#' @return a `list` of `data.frame`s
#' @export
#'
#' @examples
#'
#' df_rsplit_interval(iris, 10)
#'
df_rsplit_interval <- function(x, interval = 10){
  assert_that(is.data.frame(x))
  assert_that(rlang::is_scalar_integerish(interval))

  splits <- c(seq(1L, nrow(x), by = interval), nrow(x) + 1L)

  res <- vector("list", length(splits) - 1L)
  s <- seq_along(splits)
  s <- s[-length(s)]

  for(i in  s){
    res[[i]] <- x[splits[i]:(splits[i + 1L] - 1L), ]
  }

  res
}

