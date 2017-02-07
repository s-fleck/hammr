#' Save as
#'
#' Generic used by other STAT packages that depend on this package
#'
#' @param dat
#' @param format
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
save_as <- function(dat, outfile, format, ...){
  UseMethod('save_as')
}



#' @export
save_txt <- function(dat, outfile, overwrite = TRUE){
  UseMethod('save_txt')
}



#' @export
save_tex <- function(dat, outfile, overwrite = TRUE){
  UseMethod('save_tex')
}
