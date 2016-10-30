#' Chop up a string by position
#'
#' @param x string to chop into character vector
#' @param breaks positions at which to chop the string
#'
#' @return a character vector
#' @export
#'
#' @examples
#'
#' str_chop('123456789a', c(1,4,6,9,100))
#'
str_chop <- function(x, breaks){
  res <- vector()

  res[1] <- stringr::str_sub(x, breaks[1], breaks[2])
  for(i in 3:length(breaks)){
    res[[i-1]] <- stringr::str_sub(x, breaks[i-1]+1, breaks[i])
  }

  return(res)
}
