#' @export
all_unique <- function(x){
  length(x) %identical% length(unique(x))
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
#' @param s
#' @param strict
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
    magrittr::extract(1:(length(.) - 1)) %>%
    paste(collapse = '.')
}


#' @export
extract_file_extension <- function(x){
  res <- x %>%
    basename() %>%
    strsplit(., '.', fixed = TRUE) %>%
    unlist() %>%
    magrittr::extract(length(.) - 1)
}


#' Title
#'
#' http://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
#'
#' @param infile
#'
#' @return
#' @export
#'
#' @examples
load_rda <- function(infile){
  env <- new.env()
  nm <- load(infile, env)[1]
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

reload_hammr<- function(){
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
