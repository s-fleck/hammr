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
#' @aliases tick
#' @return tock displays current time and time difference to .tick as set by tick()
#' @export

tock = function() {
  print(paste('Tock:', Sys.time(), ' - Diff: ', format(difftime(Sys.time(), .tick), digits=2)))
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





#' Human Numbers
#'
#' Source: https://github.com/1R151-1/R/blob/master/ggplot2_formatter.r
#'
#' Format numbers so they're legible for humans
#' Use this in ggplot for labels where you might use the comma or percent functions from the
#' Scales package.
#'
#' Checks whether numbers are positive or negative.
#' Allows up to 1 significant figure
#' sapply used for element-wise application of the humanity function as a vector may include
#' numbers where billions, millions or thousands are appropriate.
#'
#' @return a character vector the same length as the input vector
#' @param x a numeric vector to format,
#' @param smbl a symbol you'd like to prefix your numbers by
#' @rdname human_numbers
#' @aliases human_gbp, human_usd, human_euro, human_num
#'
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

#' @rdname human_numbers
#' @export
human_gbp   <- function(x){human_numbers(x, smbl = "\u00A3")}

#' @rdname human_numbers
#' @export
human_usd   <- function(x){human_numbers(x, smbl = "$")}

#' @rdname human_numbers
#' @export
human_euro  <- function(x){human_numbers(x, smbl = "\u20AC")}

#' @rdname human_numbers
#' @export
human_num   <- function(x){human_numbers(x, smbl = "")}


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


#' Save object to hard disk chache
#'
#' @param ... R objects to save to chache dir, usually /inst/chache
#' @param package the current package
#' @param subdir subdirectory of the chache dir to save data to
#'
#' @section Side effects:
#' Saves an R object to a cache dir in the current package
#'
#' @seealso \code{\link{load_cache}}.
#' @export
save_cache <- function(..., pkg = '.', subdir){

  pkg       <- devtools::as.package(pkg)
  pkg_dir   <- system.file(package = pkg$package)
  cache_dir <- file.path(pkg_dir, 'extdata', 'cache')

  if(!missing(subdir)){
    cache_dir <- file.path(cache_dir, subdir)
  }

  assert_that(file.exists(pkg_dir))

  if(!file.exists(cache_dir)){
    dir.create(cache_dir, recursive = TRUE)
  }

  assert_that(file.exists(cache_dir))

  to_save     <- eval(substitute(alist(...)))


  obj         <- vapply(to_save, as.character, character(1))
  save_file   <- paste0(file.path(cache_dir, obj), '.rda')

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
  pkg       <- devtools::as.package(pkg)
  pkg_dir   <- system.file(package = pkg$package)
  cache_dir <- file.path(pkg_dir, 'extdata', 'cache')

  if(!missing(subdir)){
    cache_dir <- file.path(cache_dir, subdir)
  }

  assert_that(file.exists(pkg_dir))

  to_load    <- eval(substitute(alist(...)))
  obj        <- vapply(to_load, as.character, character(1))

  if(!missing(subdir)){
    paths     <-  paste0(file.path(cache_dir, subdir, obj), '.rda')
  } else {
    paths      <- paste0(file.path(cache_dir, obj), '.rda')
  }

  for(i in paths){
    load(i, envir = envir)
  }
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


#' Reorder character vector or levels of a factor based on priorities
#'
#' Shoves elements of a character or factor vector to the front.
#' Usefull for reordering factor levels for plotting. Issues
#' a warning if any elements of high or low are not present in x
#'
#' @param x a character of factor vector
#' @param high elements to be put to the front
#' @param low elements to be put to the back
#'
#' @rdname prioritize
#'
#' @return a reordered vector
#' @export
#' @import assertthat
#'
#' @examples
#'
#' x <- c('d', 'e', 'z', 'y', 'n', 'b', 'c', 'a', 'x')
#' prioritize(x, c('a', 'b', 'c', 'applepie'), c('x', 'y', 'z'))
#'
prioritize <- function (x, ...) {
  UseMethod("prioritize", x)
}


#' @rdname prioritize
#' @export
#'
prioritise <- prioritize


#' @rdname prioritize
#' @export
#'
prioritize.character <- function(x, high = character(), low = character()){

  low_not_x  <- low[!low %in% x]
  high_not_x <- high[!high %in% x]

  if(!all(low  %in% x)) warning('Not all "low" are present in "x": ', paste(low_not_x, collapse = ' '))
  if(!all(high %in% x)) warning('Not all "high" are present in "x": ', paste(high_not_x, collapse = ' '))

  low      <- low[low %in% x]
  high     <- high[high %in% x]
  mid      <- x[!x %in% c(high, low)]
  ordered  <- c(high, mid, low)

  return(ordered)
}


#' @rdname prioritize
#' @export
#'
prioritize.factor <- function(x, high = character(), low = character()){
  ordered <- prioritise(levels(x), high, low)
  res     <- factor(x, levels = ordered)

  return(res)
}


#' Typecast columns of a data.frame by name
#'
#' Use with care, will introduce NAs for some conversion attempts
#'
#' @param dat a data.frame
#' @param conv a list of the form list(COLNAME = 'coltype')
#'
#' @return a data.frame with typecasted columns
#' @export
#'
#' @examples
#' dat <- data.frame(foo = c('5', '6', '5'),
#'                   bar = factor(c('a', 'b', 'c')),
#'                   stringsAsFactors = FALSE)
#' str(dat)
#'
#' res <- typecast_cols(dat, list(foo = 'numeric',
#'                               bar = 'character'))
#'
#' str(dat)

typecast_cols <-  function(dat, conv = list()){

  conv2 <- conv[names(conv) %in% names(dat)]
  if(length(conv2) < length(conv)) {
    missing_cols <- paste(names(conv)[!names(conv) %in% names(conv2)], collapse = ', ')

    warning('Not all conv present in names(x): ', missing_cols)
  }

  for(i in names(conv2)){
    toclass <- conv2[[i]]

    if('POSIXct' %in% toclass){
      toclass <- 'POSIXct'
    }

    f <- cfun(toclass)

    if(any(class(dat[[i]]) != toclass)) {

    dat[[i]] <- tryCatch(f(dat[[i]]),
               warning = function(w) {
                 warning(i, '(', class(dat[[i]]), '->', toclass, '): ', w)
                 f(dat[[i]])
               }
            )

    }
  }

  return(dat)
}


#' Typecast all columns of a data.frame of a specific type
#'
#' Use with care, will introduce NAs for some conversion attempts
#'
#' @param dat a data.frame
#' @param from column type to cast
#' @param to target column type
#'
#' @return a data frame with all columns of class from converted to class to
#' @export
#' @import assertthat
#'
#' @examples
#'
#' df <- data.frame(a = factor(c('b', 'a', 'd')),
#'                  b = factor(c(1,2,3)))
#'
#' str(df)
#'
#' x <- typecast_all(df, 'factor', 'character')
#' y <- typecast_all(df, 'factor', 'numeric')
#'
#' str(x)
#' str(y)

typecast_all <- function(dat, from = 'factor', to = 'character'){

  from  <- tolower(from)
  tofun <- cfun(to)

  vars <- lapply(dat, class) == from
  dat[, vars] <- lapply(dat[, vars], tofun)
  return(dat)
}


cfun <- function(x){

  res <- switch(x,
                'logical'  = as.logical2,
                'integer'   = as.integer2,
                'factor'    = as.factor,
                'numeric'   = as.numeric2,
                'character' = as.character,
                'POSIXct'   = as.POSIXct,
                stop('Input must be any of "numeric", integer", "factor", "character"')
  )
  return(res)
}

as.numeric2 <- function(x) as.numeric(as.character(x))

as.integer2 <- function(x) as.integer(as.character(x))

as.logical2 <- function(x) as.logical(as.character(x))


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
