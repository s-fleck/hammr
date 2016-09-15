parse_raw <- function(x, format, encoding = NULL){
  assert_that(is.scalar(format))
  assert_that(is.raw(x))


  # aliases
  if(format %in% c('bigint', 'i64'))            format <- 'integer64'
  if(format %in% c('string', 'char', 'c'))      format <- 'character'
  if(format %in% c('int',  'i'))                format <- 'integer'
  if(format %in% c('double', 'd', 'num', 'n'))  format <- 'numeric'


  # Packed Decimal ----
  if(stringr::str_detect(format, 'pd')){
    assert_that(stringr::str_detect(format, '^pd.\\d+$'))

    div     <- stringr::str_split(format, '\\.') %>%
      unlist() %>%
      magrittr::extract2(2) %>%
      as.integer()

    res   <- parse_packed_decimal(x, d = div)
  }


  # Packed Integer ---
  if(identical(format, 'pi')){
    res   <- parse_packed_decimal(x, d = 0)
  }


  return(res)


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

