#' Print as
#'
#' @param dat
#' @param format
#' @param ...
#'
#' @return
#' @export
#' @import xtable stringi
#'
#' @examples
print_as <- function(dat, format, ...){   UseMethod('print_as')

}


print_as.default <- function(dat, format, ...){
  format %assert_class% 'character'
  assert_that(is.scalar(format))

  switch(format,
         'tex' = print_tex(dat, ...))
}

print_tex <- function(dat){
  UseMethod('print_tex')
}

