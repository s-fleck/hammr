#' Title
#'
#' @param dat
#' @param values
#' @param col
#' @param sort
#'
#' @return
#' @export
#'
#' @examples
df_complement <- function(dat, values, col, sort = FALSE){
  assert_that(is.scalar(col))

  dd <- data.table::copy(dat)

  if(is.factor(dat[[col]])){
    dd[[col]] <- as.character(dd[[col]])
  }

  if(is.factor(values)){
    values <- as.character(values)
  }

  all_values <- list(c(dd[[col]], values[!values %in% dat[[col]]]))
  names(all_values) <- col
  all_values <- as.data.frame(all_values)

  merge(dat, as.data.frame(all_values), by = col, all = TRUE)
}
