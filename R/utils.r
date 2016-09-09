#' String concatenation infix operator
#'
#' @param a A string
#' @param b Another string
#'
#' %_% pastes two strings together (with ' ' as sepparator)
#' %-% pastes two strings together (without sepparator)
#'
#' @return a and b pasted together
#' @export
#' @rdname paste_infix
#'
#' @examples
#'
#' "ra" %-% "ce" %_% "car"

`%_%` <- function(a, b) {
  paste(a, b)
}


#' @rdname paste_infix
#' @export
`%-%` <- function(a, b) {
  paste0(a, b)
}

# example of how to redefine the + operator for
# string concattenation
#
# http://stackoverflow.com/questions/4730551/making-a-string-concatenation-operator-in-r
#
# `+` <- function (e1, e2) {
#   UseMethod("+")
# }
#
# `+.default` <- function (e1, e2) .Primitive("+")(e1, e2)
#
# `+.character` <- function(e1, e2) {
#   if(length(e1) == length(e2)) {
#     paste(e1, e2, sep = '')
#   } else stop('String Verctors of Different Lengths')
# }
#


#' Test if all elements of a vector are identical
#'
#' http://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
#'
#' @param x vector to be tested
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#'
#' all_identical(c(1,2,3))
#' all_identical(c(1,1,1))
#'
all_identical <- function(x, warn_single_value = FALSE) {
  if (length(x) == 1L) {
    if(warn_single_value) warning("'x' has a length of only 1")
    return(TRUE)
  } else if (length(x) == 0L) {
    warning("'x' has a length of 0")
    return(logical(0))
  } else {
    TF <- vapply(1:(length(x)-1),
                 function(n) identical(x[[n]], x[[n+1]]),
                 logical(1))
    if (all(TF)) TRUE else FALSE
  }
}




#' Remove whitespace from all character (and factor) columns of a data.frame
#'
#' @param dat a data.frame
#' @param process_factors wheter or not factor labels should also be processed
#'
#' @return a data.frame
#' @export
remove_whitespace = function(dat, process_factors = FALSE){

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



#' Extract legend from a ggplot 2 object
#'
#' source: http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
#'
#' @param a.gplot a ggplot2 object
#'
#' @return a legend or an empty grob
#' @export
#'
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}




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


#' Drop a column from a data.frame if it exists
#'
#' @param dat data.frame
#' @param drop character vector of the columns that are to be dropped
#'
#' @return a data.frame without the columns specified in drop
#' @export

drop_if_exists <- function(dat, drop){

  drop <- paste0('^', drop, '$')

  rex <- paste0(drop, collapse = '|')
  #rex <- paste0('[', rex, ']')
  sel <- !grepl(rex, names(dat))

  dropped_names <- names(dat)[!sel]

  if(length(dropped_names) > 0){
    message('dropped: ', dropped_names, collapse = ', ')
  }

  res <- dat[sel]
  return(res)
}


#' Get the n most frequent values of a vector
#'
#' @param x A vector
#' @param x
#'
#' @return a character vector of the n most frequent elements
#' @export
most_frequent <- function(x, n = 1){
  if(n > length(unique(x))){
    n <- length(unique(x))
    warning('n is more than the count of unique values of x.')
  }
  res <- names(sort(table(x),decreasing=TRUE)[1:n])
  if(class(x) == 'numeric') res <- as.numeric(res)
  if(class(x) == 'integer') res <- as.integer(res)
  if(class(x) == 'logical') res <- as.logical(res)

  return(res)
}



#' Chop up a string by position
#'
#' @param x string to chop into character vector
#' @param breaks positions at which to chop the string
#'
#' @return a character vector
#' @export
#'
#' @examples
#'
#' str_chop('123456789a', c(1,4,6,9,100))
#'
str_chop <- function(x, breaks){
  res <- vector()

  res[1] <- stringr::str_sub(x, breaks[1], breaks[2])
  for(i in 3:length(breaks)){
    res[[i-1]] <- stringr::str_sub(x, breaks[i-1]+1, breaks[i])
  }

  return(res)
}




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
extract_filename_from_path <- function(x, ext = TRUE){
    res <- strsplit(x, '/') %>%
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


#' @export
`%identical%` <- identical



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

#' @export
string_pad <- function(x, width) {
  fs <- paste0('%0', width, 'd')

  res <- sprintf(fs, as.integer(x))
  res[grep('NA', fixed = TRUE, res)] <- NA
  return(res)
}
