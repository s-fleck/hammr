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
#' @import bit64
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
                'integer64' = as.integer642,
                'factor'    = as.factor,
                'numeric'   = as.numeric2,
                'character' = as.character,
                'POSIXct'   = as.POSIXct,
                stop('Input must be any of "numeric", integer", "factor", "character"')
  )
  return(res)
}


as.numeric2   <- function(x) as.numeric(as.character(x))
as.integer2   <- function(x) as.integer(as.character(x))
as.integer642 <- function(x) bit64::as.integer64(as.character(x))
as.logical2   <- function(x) as.logical(as.character(x))
