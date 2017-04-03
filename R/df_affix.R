#' Paste a suffix and/or prefix to each column of a data.frame
#'
#' @param dat a data.frame
#' @param prefix to be pasted before each element of `dat`
#' @param suffix to be pasted after each element of `dat`
#'
#' @return a \code{data.frame} (with only character columns)
#'
#' @family data.frame tools
#' @rdname df_affix
#' @md
#'
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   a = c(1, 2),
#'   b = c('a', 'b'),
#'   stringsAsFactors = FALSE
#' )
#'
#' df_affix(dat, "prefix-", "-suffix" )
#' df_parenthesis(dat)
#' df_brackets(dat)
#'
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
