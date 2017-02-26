#' @export
equal_or_both_na <- function(a, b){
  res <- a == b  | is.na(a) & is.na(b)
  res[is.na(a) & !is.na(b)] <- FALSE
  res
}




#' @export
all_with_warning <- function(dat){
  dat <- as.list(dat)
  dat %assert_class% 'list'
  assert_that(unlist(unique(lapply(dat, class))) %identical% 'logical')

  datl <- as.logical(dat)
  if(any(is.na(datl))){
    warning("Treating NAs as 'FALSE'")
  }

  dat[which(is.na(datl))] <- FALSE
  datl[is.na(datl)]       <- FALSE

  if(all(datl)){
    return(TRUE)
  } else {
    failed      <- dat[as.logical(lapply(dat, identical, FALSE))]
    warn        <- paste(
      'FALSE, but should be TRUE:\n',
      paste(names(failed), collapse = ', ')
    )
    warning(warn)
    return(FALSE)
  }
}




#' Test if all elements of a vector are identical
#'
#' @param x any object that can be handled by \code{unique} (usually a vector or
#'   list)
#' @param empty_value Value to return if function is called on a vector of
#'   length 0
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#'
#' all_identical(c(1,2,3))
#' all_identical(c(1,1,1))
#'

all_identical <- function(x, empty_value = FALSE) {
  # Check inputs
  if(length(x) <= 1L){
    if(identical(length(x), 1L)){
      warning("'x' consists of only one element")
      return(TRUE)
    } else if (identical(length(x), 0L)){
      if(is.null(x)){
        warning("'x' is NULL")
      } else {
        warning("'x' is an empty vector")
      }
      return(empty_value)
    }
  } else {
    identical(length(unique(x)), 1L)
  }
}




#' Test if all elements of a vector are unique
#'
#' @inheritParams all_identical
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#'
#' all_unique(c(1,2,3))
#' all_unique(c(1,1,1))
#'
all_unique <- function(x, empty_value = FALSE){
  if(length(x) <= 1L){
    if(identical(length(x), 1L)){
      warning("'x' consists of only one element")
      return(TRUE)
    } else if (identical(length(x), 0L)){
      if(is.null(x)){
        warning("'x' is NULL")
      } else {
        warning("'x' is an empty vector")
      }
      return(empty_value)
    }
  } else {
    identical(length(unique(x)), length(x))
  }
}




#' @export
as_readr_col <- function(dat){
  UseMethod('as_readr_col')
  }

#' @export
as_readr_col.character <- function(dat){
  switch(tolower(dat),
         'character' = readr::col_character(),
         'integer'   = readr::col_integer(),
         'numeric'   = readr::col_number())
}


#' @export
as_readr_col.list <- function(dat){
  lapply(dat, as_readr_col)
}




#' Capitalize words
#'
#' For ?toupper documentation
#'
#' @param s a character vector
#' @param strict enforce lowercase characters after first (camelCase becomes Camelcase)
#'
#' @return
#' @export
#'
#' @examples
capwords <- function(s, strict = FALSE) {
  cap <- function(s){
    s_upper <- toupper(substring(s, 1, 1))
    s_lower <- substring(s, 2)
    if(strict) s_lower <- tolower(s_lower)
    paste(s_upper, s_lower, sep = "", collapse = " " )
  }

  sapply(strsplit(s, split = " "),
         cap,
         USE.NAMES = !is.null(names(s)))
}




#' Unqiue single element
#'
#' Returns unique(x) if all elements of x are identical, raises an error if
#' not all elements of x are identical.
#'
#' @param x any object that can be handled by \code{unique} (usually a vector or
#'   list)
#'
#' @return A scalar of the same type as x
#' @export
unique_single <- function(x){
  res <- unique(x)
  if(is.scalar(res)){
    return(res)
  } else {
    stop('Not all elements of x are identical')
  }
}





#' Basename without file extension
#'
#' \describe{
#'   \item{basename_sans_ext}{\code{"myscript.R"} --> \code{"myscript"}}
#' }
#'
#' @param x A filename / file path
#'
#' @return a character vector
#' @seealso \code{\link{basename}}
#' @export
basename_sans_ext <- function(x){
  res <- x %>%
    basename() %>%
    strsplit(., '.', fixed = TRUE) %>%
    unlist() %>%
    magrittr::extract(1:(length(.) - 1)) %>%
    paste(collapse = '.')
}





#' Extract file extenstion
#'
#' \describe{
#'   \item{extract_file_ext}{\code{"myscript.R"} --> \code{"R"}}
#' }
#'
#' @export
#' @rdname basename_sans_ext
extract_file_ext <- function(x){
  res <- x %>%
    basename() %>%
    strsplit(., '.', fixed = TRUE) %>%
    unlist() %>%
    magrittr::extract(length(.) - 1)
}


#' Load an rda file and return the content
#'
#' Warning: Will not work as expected for rda files that contain several
#' objects.
#'
#' http://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
#'
#' @param infile path to an rda file
#'
#' @return The first object in infile.rda
#' @export
load_rda <- function(infile){
  env <- new.env()
  nm <- load(infile, env)[1]
  if(length(env) > 1){
    warning(sprintf(
      '%s contains more than one object. Returning only the first: %s',
      infile,
      nm
    ))
  }
  env[[nm]]
}




#' Launch explorer
#'
#' @param x
#'
#' @return
#' @export
#' @rdname launchers
#'
#' @examples
explorer <- function(x){
  dn <- dirname(x)
  shell(sprintf("explorer %s", dn), intern = FALSE, wait = FALSE)
}




#' Launch excel
#'
#' @param x
#'
#' @param excel_path
#' @rdname launchers
#'
#' @export
excel <- function(
  x,
  excel_path = '"C:/Program Files (x86)/Microsoft Office/Office14/EXCEL.EXE"'
){
  shell(paste(excel_path, x), intern = FALSE, wait = FALSE)
}




#' Change factor levels according to named character vector
#'
#' This is just a (to me) slightly more convenient interface to
#' forcats::fct_recode.
#'
#' @param x
#' @param rec
#'
#' @return
#' @export
#'
#' @examples
fct_recode2 <- function(x, rec){
  assert_that(is.vector(x) || is.factor(x))
  assert_that(is.vector(rec))
  assert_that(identical(
    length(names(rec)),
    length(rec)
  ))


  args <- vector('list', length(rec))
  for(i in seq_along(args)){
    args[[i]] <- rec[i]
  }
  args <- c(list(as.character(x)), args)

  do.call(forcats::fct_recode, args)
}



# Dirty -------------------------------------------------------------------

dirty_reload <- function(){
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/hammr'))
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/gvtool'))
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/tatool'))
}


reload_gvtool <- function(){
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/gvtool'))
}

reload_tatool <- function(){
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/tatool'))
}

reload_hammr <- function(){
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/hammr'))
}


install_gv <- function(creds = NULL){
  if (is.null(creds)) {
    creds <- ui_credentials(
      'Please your bitbucket username and password')
  }

  requireNamespace("devtools")
  requireNamespace("httr")
  requireNamespace("RCurl")
  httr::set_config(httr::config(ssl_verifypeer = 0L))

  repositories <- c(
    's_fleck/testthis',
    's_fleck/tatool',
    's_fleck/gvtool',
    's_fleck/gvroad',
    's_fleck/gvrail'
  )

  for(repository in repositories){
    devtools::install_bitbucket(
      repository,
      username = creds$user,
      password = creds$pw,
      upgrade = FALSE
      )
  }
}
