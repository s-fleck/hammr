split_raw_records <- function(x, lsep = as.raw(c(0xc7, 0xe5, 0xd2))){
  lbreaks <- c(match_vector(x, lsep))

  fl <- list(x[1:lbreaks[1]-(length(lsep)-2)])

  ol <- foreach(i = 2:length(lbreaks)) %dopar% {
    res <- x[(lbreaks[i-1]+3):(lbreaks[i]-(length(lsep)-2))]
  }

  res <- c(fl, ol)

  return(res)
}


parse_gvk_line <- function(x, fields){


}


parse_ebcdic_line <- function(x, fields){
  res <- foreach(fld = fields, .export = 'parse_ebcdic', .combine = data.table::data.table, .multicombine = TRUE) %do% {
    r <- x[fld$start:fld$end]
    r <- parse_ebcdic(r, fld$parser)
  }

  names(res) <- foreach(fl = fields, .combine = c) %do% fl$name

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
    parser <- parser_ebcdic_bin
  }


  # Simple formats
  res <- parser(x)


  return(res)
}


parser_ebcdic_char <- function(x){
  iconv(rawToChar(x), from = 'IBM500', to = 'UTF-8')
}

parser_ebcdic_bin <- function(x){
  as.character(x)
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

#' @export
pfield <- function(.name, .start, .end = .start, .parser = 'character'){
  assert_that(is.scalar(.name))
  assert_that(is.scalar(.start))
  assert_that(is.scalar(.end))
  assert_that(is.scalar(.parser))

  assert_that(looks_like_integer(.start))
  assert_that(looks_like_integer(.end))
  assert_that(.start <= .end)

  list(name = .name, start = .start, end = .end, parser = .parser)
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


