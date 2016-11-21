#' Extracts the filename from a path
#'
#' @param x  a character vector containing a path to a file
#' @param ext Wheter or not the result should include the file extension or not
#'
#' @return The filename
#' @export
#'
#' @examples
#'
#'  x <- c("C:/path/to/test.file.ext")
#'
#'  extract_filename_from_path(x)
#'  extract_filename_from_path(x, ext = FALSE)
extract_filename_from_path <- function(x, fsep = NULL, ext = TRUE){

  if(is.null(fsep)){
    fsep <- guess_fsep(x)
  }

  res <- strsplit(x, fsep) %>%
    unlist() %>%
    tail(1)

  if(!ext){
    res <- res %>%
      strsplit(., '.', fixed = TRUE) %>%
      unlist() %>%
      extract(1:(length(.)-1)) %>%
      paste(collapse = '.')
  }

  return(res)
}


extract_extension_from_path <- function(x, fsep = NULL){
  res <- extract_filename_from_path(x)

  res %>%
    strsplit(., '.', fixed = TRUE) %>%
    unlist() %>%
    extract(length(.))
}


# Utility Functions ----

guess_fsep <- function(x){

    a <- length(unlist(stringi::stri_locate_all(x, regex = '\\\\')))
    b <- length(unlist(stringi::stri_locate_all(x, regex = '/')))

    if(a > b){
      fsep <- '\\\\'
    } else {
      fsep <- '/'
    }

  return(fsep)
}
