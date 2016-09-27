#' split raw records
#'
#'
#'
#' @param x
#' @param lsep Record("Line") sepparator in the EBDIC file. This parameters has
#' to be entered as raw bits. For example, if our records are sepparated by
#' the string GVK, lsep must be as.raw(c(0xc7, 0xe5, 0xd2)). You can figure
#' out the bits of the EBCDIC equivalent of an ASCII string like this:
#'
#' lsep <- iconv('GVK', from = 'UTF-8', to = 'IBM500') %>%
#'   charToRaw()
#'
#' @return
#' @export

split_raw_records <- function(x, lsep){
  lbreaks   <- match_vector(x, lsep)
  #lbreaksw  <- which(c(match_vector(x, lsep)))

  splt    <- c(rep(FALSE, length(lsep)), lbreaks)   # Shift break boints my length of sep vector
  splt    <- cumsum(splt)
  splt    <- as.factor(splt)

  res <- split(x, splt)

  return(res)
}



#' @export
parse_ebcdic_line <- function(x, fields){
  splt <-  attr(fields, 'split')
  res  <- split(x[1:length(splt)], splt)

  res[fields$parser == 'character'] <- iconv(res[fields$parser == 'character'], from = 'ibm500', to = 'ascii')
  res[fields$parser == 'skip'] <- NULL
  res[fields$parser == 'pd.4'] <- NULL

  res <- data.table::as.data.table(res)

  return(res)
}


parse_ebcdic <- function(x, parser){
  assert_that(is.scalar(parser))
  assert_that(is.raw(x))


  # Complex parsers ----

  if(class(parser) == 'character'){
    # Packed Decimal ----
    if(stringr::str_detect(parser, 'pd')){
      assert_that(stringr::str_detect(parser, '^pd.\\d+$'))

      div     <- stringr::str_split(parser, '\\.') %>%
        unlist() %>%
        magrittr::extract2(2) %>%
        as.integer()

      res   <- parse_packed_decimal(x, d = div)
      return(res)
    }

    # Packed Integer ---
    if(identical(parser, 'pi')){
      res   <- parse_packed_decimal(x, d = 0)
      return(res)
    }
  }


  # Simple parsers -----
  if(parser %in% c('bigint', 'i64')){
    parser <- 'integer64'

  } else if(parser %in% c('character', 'string', 'char', 'c')) {
    parser <- parser_ebcdic_char

  } else if(parser %in% c('int',  'i')){
    parser <- 'integer'

  } else if(parser %in% c('double', 'd', 'num', 'n')){
      parser <- 'numeric'
  } else if(parser %in% c('x', 'binary', 'bin')){
    parser <- parser_bin
  }


  # Simple formats
  res <- parser(x)


  return(res)
}


parser_ebcdic_char <- function(x){
  iconv(rawToChar(x), from = 'IBM500', to = 'UTF-8')
}

#' @export
parser_bin <- function(x){
  as.integer(paste0('0x', paste(x, collapse = '')))
}


#' Parse packed decimal number
#'
#' The somewhat akward parameters w.d are designed after the SAS PDw.d Informat
#' description
#'
#' @param x  a raw vector
#' @param d  Number of decimal digits (the power of 10 by which to divide the value)
#' @param psign  Nibble indicating a postive sign
#' @param nsign  Nibble indicating a negative sign
#'
#' @return
#' @export
#'
#' @examples
parse_packed_decimal <- function(x, d = 0, psign = 'c', nsign = 'd'){
  x %assert_class% 'raw'
  assert_that(is.scalar(d))
  assert_that(looks_like_integer(d))

  assert_that(all(psign %in% c('a', 'b', 'c', 'd', 'e', 'f')))
  assert_that(all(nsign %in% c('a', 'b', 'c', 'd', 'e', 'f')))
  assert_that(identical(length(intersect(nsign, psign)), 0L))

  y <- paste(as.character(x), collapse = '')


  # Parse sign  & number ----
  if(!all(is.null(psign)) || !all(is.null(nsign))){
    assert_that(!is.null(nsign) && !is.null(psign))


    # get sign & number
    signNibble <- stringr::str_sub(y, nchar(y), nchar(y))
    number     <- stringr::str_sub(y, 1, nchar(y)-1)

    # parse sign
    if(signNibble %in% psign){
      sign <- 1L
    } else if (signNibble %in% nsign) {
      sign <- (-1L)
    } else {
      stop('The final nibble of the packed decimal field should contain the sign.
           Are you sure x is really a packed decimal value? If there really is
           no sign nibble, set psign and nsign to NULL.')
    }
    } else {
      sign   <-  1L
      number <-  y
    }

  assert_that(sign %in% c(-1L, 1L))
  assert_that(looks_like_integer(number))

  res <- as.numeric(number) * sign / 10^d

  return(res)
}


# Utilities ----

#' Title
#'
#' @param ...
#' @param line_length
#'
#' @return
#' @export
#'
#' @examples
pline <- function(..., line_length = NULL){
  dat <- list(...)

  list_valid <- all(unlist(lapply(dat, function(x) class(x) %identical% c('pfield', 'list'))))
  assert_that(list_valid)

  dd <- foreach(e = dat, .combine = rbind) %do%{
    data.table::data.table(start = e$start, end = e$end, name = e$name, parser = e$parser)
  }

  dd <- dd[order(start)]

  fl <- dd[1]

  if(fl$start > 1){
    nl <- data.table(start  = 1,
                     end    = fl$start - 1,
                     name   = '#fill',
                     parser = 'skip')
    dd <- rbind(nl, dd)
    dd <- dd[order(start)]
  }

  assert_that(dd$end %identical% sort(dd$end))

  res <- dd

  for(i in 2:nrow(dd)){
    prv <- dd[i-1]
    cur <- dd[i]

    assert_that(prv$end   < cur$start)
    assert_that(prv$start <= prv$end)
    assert_that(cur$start <= cur$end)

    if(prv$end < cur$start - 1){
      nl <- data.table(start = prv$end + 1,
                       end   = cur$start - 1,
                       name  = '#fill',
                       parser = 'skip')
      res <- rbind(res, nl)
    }
  }

  res <- res[order(start)]

  splitvec <- foreach(i = 1:nrow(res), .combine = c) %do% {
    r <- res[i]
    rep(r$name, (r$end - r$start + 1))
  }

  splitvec <- factor(splitvec, levels = unique(splitvec))
  assert_that(length(splitvec) %identical% as.integer(max(res$end)))

  levels(splitvec)

  attr(res, 'split') <- splitvec

  return(res)

}


#' @export
pfield <- function(.name, .start, .end = .start, .parser = 'character'){
  assert_that(is.scalar(.name))
  assert_that(is.scalar(.start))
  assert_that(is.scalar(.end))
  assert_that(is.scalar(.parser))

  assert_that(looks_like_integer(.start))
  assert_that(looks_like_integer(.end))
  assert_that(.start <= .end)

  res <- list(name = .name, start = .start, end = .end, parser = .parser)
  class(res) <- c('pfield', 'list')
  return(res)
}

#
#
#
# rawToChar(datBin[37]) %>% iconv(from = "IBM500", to = 'ASCII')
# rawToChar(datBin[255]) %>% iconv(from = "IBM500", to = 'ASCII')
# rawToChar(datBin[256]) %>% iconv(from = "IBM500", to = 'ASCII')
# rawToChar(datBin[257]) %>% iconv(from = "IBM500", to = 'ASCII')
# rawToChar(datBin[258]) %>% iconv(from = "IBM500", to = 'ASCII')
#
# x <- datBin[33:36]
# x
#
# datBin[255:257]
#
#
#
# x <- rawToChar(x[[23]])
#
# iconv(x, from = "IBM500", to = 'UTF-8')
#
#
# binLine <- datBin[1:258]
# rawToChar(binLine) %>% iconv(from = "IBM500", to = 'ASCII')
#
#
# for(field in fields){
#   sub <- binLine[field$start:field$end]
#
#   if(grepl('pd', field$format)){
#     paste(sub, collapse = '') %>% print()
#   } else {
#     rawToChar(sub) %>% iconv(from = "IBM500", to = 'ASCII') %>% print()
#   }
# }
#
#
# readHostBin <- function(dat, fields, lsep)
#
#   # jahr 2. monat 2. bermo 2. §12 hrf pd5.5 §18 kzs $9. wo 2.
#   # bdl 1. nlkl 1. z $2. gew pd4.4 fw 1. gru 1. geow 1. +10
#   # lkw 1. §54 kfzk $8. fg 1. +8 nkg 6. +2 kmbeg 6. kmend 6. §91 strukt $1.
#   # §93 fkminl 4. fkmaus 4. fkmges 4.
#   # §105 fva 2. anz pib2. §;




