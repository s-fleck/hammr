#' Chop up a string by position
#'
#' @param x string to chop into character vector
#' @param breaks An numeric vector of positions at which to chop the string. If
#'   the first element is >1, the characters up to that point will be discarded.
#'   If there are breaks for ranges greater than `length(x)`, the results
#'   vector will contain empty strings (`""`).
#'
#' @return a character vector of length `breaks-1`
#'
#' @md
#' @export
#' @family string tools
#' @examples
#'
#' str_chop('123456789a', c(1,4,6,9,100))
#' # [1] "1234" "56"   "789"  "a"
#'
#' str_chop('123456789a', c(2,4,6,9,100, 105))
#' # [1] "234" "56"  "789" "a"   ""
#'
str_chop <- function(x, breaks){
  res <- vector()

  res[1] <- stringr::str_sub(x, breaks[1], breaks[2])
  for(i in 3:length(breaks)){
    res[[i-1]] <- stringr::str_sub(x, breaks[i-1]+1, breaks[i])
  }

  return(res)
}
