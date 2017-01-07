#' Display info about an R object with attributes
#'
#' @param dat an R object
#' @export
info <- function(dat){

  msg <- attr(dat, 'info')
  if(!is.null(msg)) cat(msg)
}


#' Return a list of usefull regex patterns
#'
#' @return a list
#' @export
rexpat <- function(){
  list(
    valid_urs	=	'[Z,R,K,S,U,L,F,G]\\d{3}[A-Z]\\d{3}.'
  )
}

#' @export
as_readr_col <- function(dat){ UseMethod('as_readr_col')}

#' @export
as_readr_col.character <- function(dat){
  switch(tolower(dat),
         'character' = readr::col_character(),
         'integer'   = readr::col_integer(),
         'numeric'   = readr::col_number())
}


#' @export
as_readr_col.list <- function(dat){
  res <- lapply(dat, as_readr_col)
}



#' Capitalize words
#'
#' For ?toupper documentation
#'
#' @param s
#' @param strict
#'
#' @return
#' @export
#'
#' @examples
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


#' @export
unique_single <- function(x){
  res <- unique(x)
  if(is.scalar(res)){
    return(res)
  } else {
    stop('Not all elements of x are identical')
  }
}


#' @export
basename_sans_ext <- function(x){
  res <- x %>%
    basename() %>%
    strsplit(., '.', fixed = TRUE) %>%
    unlist() %>%
    magrittr::extract(1:(length(.)-1)) %>%
    paste(collapse = '.')
}


#' @export
extract_file_extension <- function(x){
  res <- x %>%
    basename() %>%
    strsplit(., '.', fixed = TRUE) %>%
    unlist() %>%
    magrittr::extract(length(.)-1)
}
