#' Title
#'
#' @param x
#' @param interval
#'
#' @return
#' @export
#'
#' @examples
df_rsplit_interval <- function(x, interval = 10){
  splits <- c(seq(1, nrow(x), by = interval), nrow(x) + 1)

  res <- vector("list", length(splits) - 1)
  s <- seq_along(splits)
  s <- s[-length(s)]

  for(i in  s){
    res[[i]] <- x[splits[i]:(splits[i + 1] - 1), ]
  }

  res
}
