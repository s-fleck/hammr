#' Paste a suffix or prefix to each column of a data.frame
#'
#' @param dat
#' @param prefix
#' @param suffix
#'
#' @return
#' @export
#' @rdname df_affix
#'
#' @examples
df_affix <- function(dat, prefix, suffix){

  fun   <- function(x, p= prefix, s = suffix) paste0(p, x, s)
  dat[] <- lapply(dat, fun)
  return(dat)
}


#' @export
#' @rdname df_affix
df_parenthesis <- function(dat){
  df_affix(dat, '(', ')')
}

#' @export
#' @rdname df_affix
df_brackets <- function(dat){
  df_affix(dat, '[', ']')
}

#' @export
#' @rdname df_affix
df_braces <- function(dat){
  df_affix(dat, '{', '}')
}
