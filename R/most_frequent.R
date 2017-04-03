#' Get the n most frequent values of a vector
#'
#' @param x A vector
#' @param n number of most frequent elements to get
#'
#' @return a character vector of the n most frequent elements
#' @export
most_frequent <- function(x, n = 1){
  if(n > length(unique(x))){
    n <- length(unique(x))
    warning('n is more than the count of unique values of x.')
  }
  res <- names(sort(table(x),decreasing=TRUE)[1:n])
  if(class(x) == 'numeric') res <- as.numeric(res)
  if(class(x) == 'integer') res <- as.integer(res)
  if(class(x) == 'logical') res <- as.logical(res)

  return(res)
}
