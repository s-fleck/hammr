#' A custom data type for Year-Quarter
#'
#' @param y year or a character string of a format like this: 2013-Q1
#' @param q quarter (optional)
#'
#' @return An object of type Quarter, character
#' @export
#'
#' @examples
#'
#' quarter(2013, 3)
#' quarter('2013-Q3')

quarter <- function(y, q) {

  y <- as.character(y)

  if(missing(q)) {
    assert_that(all(grepl('\\d{4}-Q[1-4]', y)))
    res <- y
  } else {
    assert_that(length(y) == length(q))
    q   <- as.character(q)
    res <- paste0(y, '-Q', q )
  }

  class(res) <- c('Quarter', 'character')

  return(res)
}


# S3 Methods

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


as.data.frame.Quarter <- function(x){
  dat <- data.frame(x = as.vector(x)) %>%
    tidyr::separate(col = x, into = c('y', 'q'), sep = '-Q')
}


as.integer.Quarter <- function(x){
  as.data.frame(x) %>%
    tidyr::unite('col', y, q, sep = '') %>%
    extract2(1) %>%
    as.integer()
}



increment.Quarter <- function(x, inc){
  res <- x %>%
    as.data.frame() %>%
    dplyr::mutate(
      y = as.integer(y),
      q = as.integer(q)
    )

  res$q <- res$q + inc

  while(any(res$q > 4)){
    res$y[res$q > 4] <- res$y[res$q > 4] + 1
    res$q[res$q > 4] <- res$q[res$q > 4] - 4
  }

  while(any(res$q < 1)){
    res$y[res$q < 1] <- res$y[res$q < 1] - 1
    res$q[res$q < 1] <- res$q[res$q < 1] + 4
  }

  quarter(res$y, res$q)
}



#' as.Date method for Quarter
#'
#' @export

as.Date.Quarter <- function(x){
  dat <- as.data.frame(x)

  dat$m = 1
  dat$m[dat$q == 2] = 4
  dat$m[dat$q == 3] = 7
  dat$m[dat$q == 4] = 10

  paste(dat$y, dat$m, 01, sep = '-') %>%
    as.Date()
}


#' Convert quarter to date
#'
#' @param y dgssd
#' @param q sdgsd
#' @export
quarter_as_date <- function(y, q){
  res <- quarter(y, q) %>%
    as.Date()

  return(res)
}
