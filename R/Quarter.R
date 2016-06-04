#' A custom data type for Year-Quarter date format
#'
#' @param y
#' @param q
#'
#' @return An object of type Quarter, character
#' @export

Quarter <- function(y, q){

  y <- as.character(y)

  if(missing(q)){
    if(grep('\\d{4}-Q[1-4]', y)){
      res <- y
    } else {
      stop('Please enter y, q or jjjj-Qq')
    }
  } else {
    q <- as.character(q)
    res <- paste0(y, '-Q', q )
  }

  class(res) <- c('Quarter', 'character')
  return(res)
}


`<.Quarter` <- function(x, y){
  as.integer(x) < as.integer(y)
}


`<=.Quarter` <- function(x, y){
  as.integer(x) <= as.integer(y)
}


`>.Quarter` <- function(x, y){
  as.integer(x) > as.integer(y)
}


`>=.Quarter` <- function(x, y){
  as.integer(x) >= as.integer(y)
}


as.integer.Quarter <- function(x){
  strsplit(x, '-Q')[[1]] %>%
    paste0(collapse = '') %>%
    as.integer()
}
