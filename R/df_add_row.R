#' Manually add a row to a data.frame
#'
#' @param x a `data.frame` or `data.table`
#' @param ... Named values whose names match the names of `x`
#'
#' @return a `data.frame` or `data.table`
#' @export
#'
#' @examples
#' df_add_row(iris, Species = "blubb")
df_add_row <- function(
  x,
  ...
){
  assert_namespace("data.table")
  assert(is.data.frame(x))

  dots <- list(...)

  a <- data.table::as.data.table(dots)
  assert(is.null(names(dots)) || all(names(a) %in% names(x)) )

  res <- data.table::rbindlist(
    list(x, a), fill = TRUE, use.names = TRUE
  )

  if (data.table::is.data.table(x)){
    return(res)
  } else {
    return(as.data.frame(res))
  }
}
