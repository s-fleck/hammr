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


  as.data.frame.Quarter <- function(x, ...){
    dat <- data.frame(x = as.vector(x)) %>%
      tidyr::separate(col = x, into = c('y', 'q'), sep = '-Q')
  }


as.integer.Quarter <- function(x){
  x <- as.character(x)
  x <- gsub('-Q', '', x)
  as.integer(x)
}


is_valid.Quarter <- function(x)(
  FALSE
)


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
#' Shorthand function for converting year - qaurter date to
#'
#' @param y Year
#' @param q Quarter
#' @export
quarter_as_date <- function(y, q){
  warning('deprecated')
  res <- quarter(y, q) %>%
    as.Date()

  return(res)
}
