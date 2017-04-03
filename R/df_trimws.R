#' Remove whitespace from all character (and factor) columns of a data.frame
#'
#' @param dat a data.frame
#' @param process_factors whether or not factor labels should also be trimmed
#'
#' @return a data.frame
#'
#' @family data.frame tools
#' @export
df_trimws = function(dat, process_factors = FALSE){
  dat %assert_class% 'data.frame'

  for(i in seq_along(dat)){
    if('character' %in% class(dat[[i]])){
      dat[[i]] <- trimws(dat[[i]])
    }
    if(process_factors && 'factor' %in% class(dat[[i]])){
      levels(dat[[i]]) <- trimws(levels(dat[[i]]))
    }
  }

  return(dat)
}
