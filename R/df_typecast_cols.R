#' Typecast columns of a data.frame by name
#'
#' Bulk convert columns of a data.frame to the data.types specified in a list.
#' Use with care, will introduce NAs for some conversion attempts
#'
#' @param dat a data.frame
#' @param conv a list of the form list(COLNAME = 'coltype')
#'
#' @return a data.frame with typecasted columns
#' @export
#' @import bit64
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

df_typecast_cols <-  function(dat, conv = list()){

  conv2 <- conv[names(conv) %in% names(dat)]
  if(length(conv2) < length(conv)) {
    missing_cols <- paste(names(conv)[!names(conv) %in% names(conv2)],
                          collapse = ', ')

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
                             warning(i, '(', class(dat[[i]]), '->',
                                     toclass, '): ', w)
                             f(dat[[i]])
                           }
      )

    }
  }

  return(dat)
}


#' Typecast all columns of a data.frame of a specific type
#'
#' Bulk convert columns of a data.frame that share a certain class to a different
#' class. Use with care, will introduce NAs for some conversion attempts
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

df_typecast_all <- function(dat, from = 'factor', to = 'character'){
  dat   <- as.data.frame(dat)
  tofun <- cfun(to)

  vars <- names(dat)[unlist(lapply(dat, class) == from)]

  for(i in vars){
    dat[[i]] <- tofun(dat[[i]])
  }

  return(dat)
}


cfun <- function(x){

  msg <- paste('Input must be any of "numeric", integer", "factor"',
               '"character", "POSIXct", "integer64", "Date", but is', x)

  res <- switch(x,
                'logical'   = as.logical,
                'integer'   = as.integer2,
                'integer64' = as.integer642,
                'factor'    = as.factor,
                'numeric'   = as.numeric2,
                'character' = as.character,
                'POSIXct'   = as.POSIXct,
                'Date'      = as.Date,
                stop(msg)
  )
  return(res)
}


as.numeric2   <- function(x) as.numeric(as.character(x))
as.integer2   <- function(x) as.integer(as.character(x))
as.integer642 <- function(x) bit64::as.integer64(as.character(x))


#' @export
typecast_cols <- df_typecast_cols

#' @export
typecast_all  <- df_typecast_all


