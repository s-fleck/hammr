#' Print as
#'
#' @param dat
#' @param format
#' @param ...
#'
#' @return
#' @export
#' @import xtable
#'
#' @examples
print_as <- function(dat, format, ...){   UseMethod('print_as')

}

print_tex <- function(dat){
  UseMethod('print_tex')
}
