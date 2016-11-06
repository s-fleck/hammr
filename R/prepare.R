#' Metafunction to prepare data sets after reading them in
#'
#' @param dat
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
prepare <- function(dat, ...){
  UseMethod('prepare')
}
