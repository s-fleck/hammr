#' Prepare SPDF for ggplot2
#'
#' @param sp a sp object
#' @param region a region identifier
#'
#' @return a data.frame
#' @export
#'
ggplotify_spdf = function(sp, region){
  sp.f =  fortify(sp, region = region)
  res = merge(sp.f, sp@data, by.x = 'id', by.y = region)

  return(res)
}
