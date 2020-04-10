#' Human Numbers
#'
#' Format numbers so they're legible for humans. Use this in ggplot for labels
#' where you might use the comma or percent functions from the Scales package.
#'
#' Comes with convenience functions for currencies, SI symbols and and german
#' axis labels.
#'
#' Inspired by: https://github.com/1R151-1/R/blob/master/ggplot2_formatter.r
#'
#' @param x a numeric vector
#' @param symbol a prefix symbol, useful for currencies
#' @param pots A named numeric vector of rounding levels and associated
#'   shorthand
#' @param big_mark thousands-sepparator mark
#' @param sep sepparator between number and prefix
#'
#' @return a character vector the same length as the input vector
#'
#' @rdname human_numbers
#' @export
#'
#' @examples
#' human_numbers(c(1000000 , 1500000, 10000000000))
#' human_numbers(c(1.200000e+05, -2.154660e+05, 2.387790e+05, 4.343500e+04 ,5.648675e+12), "$")
#'
#' \dontrun{
#' x <- data.frame(
#'   x = letters[sample(1:5, 1000, replace = TRUE)],
#'   y = c(runif(1000, min = -1e12, max = 1e12))
#' )
#'
#' p <- ggplot(x, aes(x = x, y = y)) + geom_point()
#' p + scale_y_continuous(labels = human_num)
#' p + scale_y_continuous(labels = human_euro)
#' }
human_numbers <- function(
  x,
  symbol   = "",
  pots     = c('k' = 1e3, 'm' = 1e6, 'b' = 1e9),
  sep      = '',
  big_mark = ','
){
  assert_that(is.numeric(x))
  assert_that(is.scalar(symbol))
  assert_that(identical(sort(pots), pots))
  assert_that(is.scalar(big_mark) && is.character(big_mark))

  rounder <- function(y) {
    round(y / 0.1) * 0.1
  }

  formatter  <- function(y, big_mark) {
    format(y, big.mark = big_mark, scientific = FALSE, trim = TRUE)
  }

  humanize <- function(y, pots, symbol){
    # Deal with currency symbols
    sign_y <- sign(y)
    if(identical(sign_y, -1)){
      prefix <- paste0('-', symbol)
    } else {
      prefix <- symbol
    }

    if(abs(y) < min(pots)){
      return(paste(prefix, y))
    }

    # Format and Round
    for(i in rev(seq_along(pots))){
      res <- rounder(abs(y) / pots[[i]])

      if (res >= 1 && res < 1e3) {
        return(paste0(
          prefix,
          res,
          sep,
          names(pots)[[i]]
        ))
      } else if(abs(res) >= 1e3){
        return(paste0(
          prefix,
          formatter(res, big_mark = big_mark),
          sep,
          names(pots)[[i]]
        ))
      }
    }

    stop('something went wrong')
  }



  zero_unit <- paste0(sep, names(pots)[which(pots == 0)])
  if (!length(zero_unit))
    zero_unit <- ""

  if (!is_blank(symbol))
    symbol <- symbol

  humanity  <- function(y, pots, symbol){
    if(is.na(y)){
      return(NA_character_)
    } else if (y == 0){
      return(paste0(symbol, "0", zero_unit))
    } else if (data.table::between(y, 0, 1)){
      return(paste0(signif(y, 2), zero_unit))
    } else {
      humanize(y, pots, symbol)
    }
  }

  vapply(x, humanity, FUN.VALUE = character(1), pots, symbol)
}




#' @rdname human_numbers
#' @export
human_gbp   <- function(x) human_numbers(x, symbol = "\u00A3")


#' @rdname human_numbers
#' @export
human_usd   <- function(x) human_numbers(x, symbol = "$")


#' @rdname human_numbers
#' @export
human_euro  <- function(x) human_numbers(x, symbol = "\u20AC")


#' @rdname human_numbers
#' @export
human_num   <- function(x) human_numbers(x, symbol = "")


#' @rdname human_numbers
#' @export
human_num_de   <- function(x, sep = " "){
  human_numbers(
    x,
    symbol = "",
    sep = sep,
    pots = c('Tsd' = 1e3, 'Mio' = 1e6, 'Mrd' = 1e9)
  )
}


#' @rdname human_numbers
#' @export
human_si   <- function(x){
  human_numbers(
    x,
    symbol = "",
    sep = "",
    pots = c(
      'K' = 1e3,
      'M' = 1e6,
      'G' = 1e9,
      'T' = 1e12,
      'P' = 1e15,
      'E' = 1e18,
      'Z' = 1e21,
      'Y' = 1e24
    )
  )
}


#' @rdname human_numbers
#' @export
human_mem <- function(x){
  human_numbers(
    x,
    symbol = "",
    sep = ' ',
    pots = c(
      'B'   = 1,
      'KiB' = 1014,
      'MiB' = 1014^2,
      'GiB' = 1014^3,
      'TiB' = 1014^4,
      'PiB' = 1014^5,
      'EiB' = 1014^6,
      'ZiB' = 1014^7,
      'YiB' = 1014^8
    )
  )
}




#' @rdname human_numbers
#' @param x a `integer` vector (containing seconds) or a [difftime] object
#' @export
#' @examples
#' human_time(350)
human_time <- function(x){
  UseMethod("human_time")
}




#' @export
human_time.difftime <- function(x){
  units(x) <- "secs"
  human_time(as.numeric(x))
}




#' @export
human_time.numeric <- function(x){
  human_time(as.integer(round(x)))
}




#' @export
human_time.integer <- function(x){
  hammr::human_numbers(
    as.integer(x),
    pots = c(s = 0L, m = 60L, h = 3600L, d = 86400L, y = 31104000L)
  )
}
