#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
statcube <- function(dat){
  dat <- copy(dat)
  dat <- as.data.table(dat)
  setattr(dat, 'class', union('Statcube', class(dat)))
  return(dat)
}


#' Title
#'
#' @param dat
#' @param target
#' @param aux
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as_statcube <- function(dat, target, aux, ...){
  UseMethod('as_statcube')
}
