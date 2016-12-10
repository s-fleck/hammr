#' Remove whitespace from all character (and factor) columns of a data.frame
#'
#' @param dat a data.frame
#' @param process_factors wheter or not factor labels should also be processed
#'
#' @return a data.frame
#' @export
df_trimws = function(dat, process_factors = FALSE){
  dat %assert_class% 'data.frame'

  for(i in 1:length(dat)){
    if('character' %in% class(dat[[i]])){
      dat[[i]] = trimws(dat[[i]])
    }
    if(process_factors && 'factor'    %in% class(dat[[i]])){
      levels(dat[[i]]) = trimws(levels(dat[[i]]))
    }
  }

  return(dat)
}

#' @export
remove_whitespace <- function(dat, process_factors = FALSE){
  warning('deprecated. use df_trimws')

  df_trimws(dat = dat, process_factors = process_factors)
  }
