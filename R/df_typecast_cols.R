#' Typecast columns of a data.frame by name
#'
#' Bulk convert columns of a data.frame to the data.types specified in a list.
#' Use with care, will introduce NAs for some conversion attempts
#'
#' @param dat a data.frame
#' @param conv a list of the form list(COLNAME = 'coltype')
#' @param silent logical. muffle warnings
#'
#' @family data.frame tools
#' @return a data.frame with typecasted columns
#' @export
#'
#' @examples
#'
#' dat <- data.frame(
#'   foo = c('5', '6', '5'),
#'   bar = factor(c('a', 'b', 'c')),
#'   stringsAsFactors = FALSE
#' )
#'
#' str(dat)
#'
#' res <- df_typecast_cols(
#'   dat,
#'   list(foo = 'numeric', bar = 'character')
#' )
#'
#'
#' str(dat)

df_typecast_cols <-  function(dat, conv = list(), silent = FALSE){

  conv2 <- conv[names(conv) %in% names(dat)]

  if((length(conv2) < length(conv)) && !silent) {
    missing_cols <- names(conv)[!names(conv) %in% names(conv2)]
    warning(defined_column_is_missing_warning(missing_cols))
  }

  for(i in names(conv2)){
    toclass <- conv2[[i]]

    if('POSIXct' %in% toclass){
      toclass <- 'POSIXct'
    }

    f <- cfun(toclass)

    if(any(class(dat[[i]]) != toclass)) {

      dat[[i]] <- tryCatch(
        f(dat[[i]]),
        warning = function(w) {
          warning(typecast_produces_na_warning(
            i,
            class(dat[[i]]),
            toclass,
            w$message
          ))
          suppressWarnings(f(dat[[i]]))
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
#' @return a data frame with all columns of class `from` converted to class `to`
#'
#' @md
#' @family data.frame tools
#' @export
#'
#' @examples
#'
#' df <- data.frame(a = factor(c('b', 'a', 'd')),
#'                  b = factor(c(1,2,3)))
#'
#' str(df)
#'
#' x <- df_typecast_all(df, 'factor', 'character')
#' y <- df_typecast_all(df, 'factor', 'numeric')
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


# Conditions --------------------------------------------------------------
defined_column_is_missing_warning <- function(missing_cols) {
  mcs <- paste(missing_cols, collapse = ', ')
  msg <- sprintf(
    'Not all columns defined in conv are present in names(x): %s',
    mcs)

  condition(c('defined_column_is_missing_warning', 'warning'),
            message = msg)
}


typecast_produces_na_warning <- function(col, fclass, tclass, text) {

  msg <- sprintf('%s(%s->%s): %s', col, fclass, tclass, text)

  condition(c('typecast_produces_na_warning', 'warning'),
            message = msg)
}



# Helpers -----------------------------------------------------------------

cfun <- function(x){

  msg <- paste('Input must be any of "numeric", integer", "factor"',
               '"character", "POSIXct", "integer64", "Date", but is', x)

  res <- switch(
    x,
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
as.integer2   <- function(x) {
  if (is.factor(x)){
    as.integer(as.character(x))
  } else {
    as.integer(x)
  }
}

as.integer642 <- function(x) {
  if(requireNamespace('bit64')){
    bit64::as.integer64(as.character(x))
  } else {
   stop('Requires the package bit64')
  }
}
