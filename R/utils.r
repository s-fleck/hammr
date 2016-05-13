#' Title
#'
#' http://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
all_identical <- function(x) {
  if (length(x) == 1L) {
    warning("'x' has a length of only 1")
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



#' Tick-Tock timer
#'
#' Convenient function to quickly measure time differences
#'
#' @aliases tock
#'
#' @return tick() displays current time and saves it to the hidden vector .tick in the global environment
#' @export
#'
#' @rdname ticktock
#'
#' @examples
#' tick()
#' # [1] "Tick: 2015-10-28 15:45:12"
#' tock()
#' # [1] "Tock: 2015-10-28 15:45:17  - Diff:  4.1 secs"
#'
tick = function() {
  .tick <<- Sys.time()
  print(paste('Tick:', .tick))
}

#' @rdname ticktock
#' @return tock displays current time and time difference to .tick as set by tick()
#' @export

tock = function() {
  print(paste('Tock:', Sys.time(), ' - Diff: ', format(difftime(Sys.time(), .tick), digits=2)))
}


#' Title
#'
#' @param dat
#' @param from
#' @param to
#'
#' @return
#' @export
#' @import assertthat
#'
#' @examples
typecast_all <- function(dat, from = 'factor', to = 'character'){

  from = tolower(from)

  assert_that(from %in% c('character', 'factor')) #logical', 'numeric', 'integer',

  tofun <- switch(to,
         #'logical'   = as.logical(),
         'factor'    = factor(),
         'numeric'   = function(x) as.numeric(as.character(x)),
         'integer'   = function(x) as.integer(as.character(x)),
         'character' = function(x) as.character(x),
         stop('Only factor to character conversion supported yet')
         )


  vars <- lapply(dat, class) == from
  dat[, vars] <- lapply(dat[, vars], tofun)

  return(dat)
}


remove_whitespace = function(dat){

  for(i in 1:length(dat)){
     if('character' %in% class(dat[[i]])) dat[[i]] = trimws(dat[[i]])
  }

  return(dat)
}


# ---------------------------------------------------------------------------------------------
# Formatting functions for ggplot  graph axis
# ---------------------------------------------------------------------------------------------
#
# Copied from: https://github.com/1R151-1/R/blob/master/ggplot2_formatter.r

#' Human Numbers: Format numbers so they're legible for humans
#' Use this in ggplot for labels where you might use the comma or percent functions from the
#' Scales package.
#'
#' Checks whether numbers are positive or negative.
#' Allows up to 1 significant figure
#' sapply used for element-wise application of the humanity function as a vector may include
#' numbers where billions, millions or thousands are appropriate.
#'
#' @return a character vector the same length as the input vector
#' @param x a numeric vector to format, smbl a symbol you'd like to prefix your numbers by
#' @examples
#' human_numbers(c(1000000 , 1500000, 10000000000))
#' human_numbers(c(1.200000e+05, -2.154660e+05, 2.387790e+05, 4.343500e+04 ,5.648675e+12), "$")
#' ggplot2 + scale_y_continuous(labels = human_numbers)
#' ggplot2 + scale_x_continuous(labels = human_numbers)
#' ggplot2 + scale_x_continuous(labels = human_gbp)

human_numbers <- function(x = NULL, smbl =""){
  humanity <- function(y){

    if (!is.na(y)){

      b <- plyr::round_any(abs(y) / 1000000000, 0.1)
      m <- plyr::round_any(abs(y) / 1000000, 0.1)
      k <- plyr::round_any(abs(y) / 1000, 0.1)

      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }

      if  ( m < 1){
        paste (y_is_positive, smbl,  k , "k", sep = "")
      } else if (b < 1){
        paste (y_is_positive, smbl, m ,"m", sep = "")
      } else {
        paste (y_is_positive, smbl,  comma(b), "b", sep = "")
      }
    }
  }

  sapply(x,humanity)
}

#' Human versions of large currency numbers - extensible via smbl

human_gbp   <- function(x){human_numbers(x, smbl = "\u00A3")}
human_usd   <- function(x){human_numbers(x, smbl = "$")}
human_euro  <- function(x){human_numbers(x, smbl = "\u20AC")}
human_num   <- function(x){human_numbers(x, smbl = "")}



# from http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}




#' Prepare SPDF for ggplot2
#'
#' @param sp
#' @param region
#'
#' @return
#' @export
#'
#' @examples
ggplotify_spdf = function(sp, region){
  sp.f =  fortify(sp, region = region)
  res = merge(sp.f, sp@data, by.x = 'id', by.y = region)

  return(res)
}


#' Save object to hard disk chache
#'
#' @param ... R objects to save to chache dir, usually /inst/chache
#' @param package the current package
#'
#' @section Side effects:
#' Saves an R object to a cache dir in the current package
#'
#' @seealso \code{\link{load_cache}}.
#' @export
save_cache <- function(..., pkg = '.', subdir){

  pkg <- devtools::as.package(pkg)

                        cache_dir   <- system.file("cache", package = pkg$package)
  if(!missing(subdir))  cache_dir   <- system.file(cache_dir, subdir)

  to_save     <- eval(substitute(alist(...)))
  obj         <- vapply(to_save, as.character, character(1))
  save_file   <-  paste0(file.path(cache_dir, obj), '.rda')

  message('Saving to ', save_file)

  save(..., file = save_file)
}


#' Load object from hard disk chache
#'
#' @param ... R objects to load from cache dir, usually /inst/chache
#' @param envir target environment
#' @param package the current package
#'
#' @section Side effects:
#' Loads an R object from a chache dir in the current package
#'
#' @seealso \code{\link{save_cache}}.
#' @export
load_cache <- function(..., pkg = '.', subdir, envir = globalenv()){

  pkg        <- devtools::as.package(pkg)
  cache_dir  <- system.file("cache", package = pkg$package)
  to_load    <- eval(substitute(alist(...)))
  obj        <- vapply(to_load, as.character, character(1))

  paths      <- paste0(file.path(cache_dir, obj), '.rda')

  if(!missing(subdir)){
    paths     <-  paste0(file.path(cache_dir, subdir, obj), '.rda')
  }

  for(i in paths){
    load(i, envir = envir)
  }
}



#' Drop a column from a data.frame if it exists
#'
#' @param dat
#' @param drop
#'
#' @return
#' @export
#'
#' @examples
drop_if_exists <- function(dat, drop){
  rex <- paste(drop, collapse = '|')
  sel <- !grepl(rex, names(dat))
  message('dropped: ', paste0(names(dat)[!sel], collapse = ', '))

  return(dat)
}




#' Get the n most frequent values of a vector
#'
#' @param x A vector
#' @param x
#'
#' @return
#' @export
#'
#' @examples
most_frequent <- function(x, n = 1){
  names(sort(table(x),decreasing=TRUE)[n])
}



str_chop <- function(x, breaks){
  res <- vector()

  res[1] <- stringr::str_sub(x, breaks[1], breaks[2])
  for(i in 3:length(breaks)){
    res[[i-1]] <- stringr::str_sub(x, breaks[i-1]+1, breaks[i])
  }

  return(res)
}



