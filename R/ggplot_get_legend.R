#' Extract legend from a ggplot 2 object
#'
#' source: http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
#'
#' @param dat a ggplot2 object
#'
#' @return a legend or an empty grob
#' @export
#'
get_legend <- function(dat) {
  tmp    <- ggplot_gtable(ggplot_build(dat))
  leg    <- which(lapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


g_legend <- function(dat){
  warning('Deprecated. use get_legend instead.')
  get_legend(dat)
}
