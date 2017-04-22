#' Extract legend from a ggplot 2 object
#'
#' source: http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
#'
#' @param x a ggplot2 object
#'
#' @return a legend or an empty grob
#' @export
#'
get_legend <- function(x) {
  requireNamespace('ggplot2')
  grobs <- x %>%
    ggplot2::ggplot_build() %>%
    ggplot2::ggplot_gtable() %>%
    magrittr::extract2('grobs')

  sel <- which(purrr::map_lgl(grobs, function(x) x$name == "guide-box"))

  grobs[[sel]]
}
