
#' Human Numbers
#'
#' Source: https://github.com/1R151-1/R/blob/master/ggplot2_formatter.r
#'
#' Format numbers so they're legible for humans
#' Use this in ggplot for labels where you might use the comma or percent functions from the
#' Scales package.
#'
#' Checks whether numbers are positive or negative.
#' Allows up to 1 significant figure
#' sapply used for element-wise application of the humanity function as a vector may include
#' numbers where billions, millions or thousands are appropriate.
#'
#' @return a character vector the same length as the input vector
#' @param x a numeric vector to format,
#' @param smbl a symbol you'd like to prefix your numbers by
#' @rdname human_numbers
#' @aliases human_gbp, human_usd, human_euro, human_num
#'
#' @examples
#' human_numbers(c(1000000 , 1500000, 10000000000))
#' human_numbers(c(1.200000e+05, -2.154660e+05, 2.387790e+05, 4.343500e+04 ,5.648675e+12), "$")
#' ggplot2 + scale_y_continuous(labels = human_numbers)
#' ggplot2 + scale_x_continuous(labels = human_numbers)
#' ggplot2 + scale_x_continuous(labels = human_gbp)

human_numbers <- function(x = NULL, smbl ="", shorts = c('k', 'm', 'b')){
  humanity <- function(y){
    if (!is.na(y)){
      b <- plyr::round_any(abs(y) / 1000000000, 0.1)
      m <- plyr::round_any(abs(y) / 1000000, 0.1)
      k <- plyr::round_any(abs(y) / 1000, 0.1)

      if (y >= 0){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }

      if (k < 1){
        paste(y_is_positive, smbl,  abs(y), sep = "")
      } else if (m < 1){
        paste (y_is_positive, smbl,  k , shorts[1], sep = "")
      } else if (b < 1){
        paste (y_is_positive, smbl, m , shorts[2], sep = "")
      } else {
        paste (y_is_positive, smbl,  comma(b), short[3], sep = "")
      }
    }
  }

  sapply(x, humanity)
}

#' @rdname human_numbers
#' @export
human_gbp   <- function(x){human_numbers(x, smbl = "\u00A3")}

#' @rdname human_numbers
#' @export
human_usd   <- function(x){human_numbers(x, smbl = "$")}

#' @rdname human_numbers
#' @export
human_euro  <- function(x){human_numbers(x, smbl = "\u20AC")}

#' @rdname human_numbers
#' @export
human_num   <- function(x){human_numbers(x, smbl = "")}

#' @rdname human_numbers
#' @export
human_num_de   <- function(x){human_numbers(x, smbl = "",  shorts = c(' Tsd', ' Mio', ' Mrd'))}
