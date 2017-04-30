#' Use C-style formating on each column of a data.frame
#'
#' @param dat a data.frame
#' @inheritParams base::sprintf
#'
#' @return a \code{data.frame} (with only character columns)
#'
#' @family data.frame tools
#' @md
#'
#' @seealso [base::sprintf()]
#' @export
#'
#' @examples
#'
#' df_sprintf(iris, "prefix-%s-suffix")
#' df_parenthesis(iris)
#' df_brackets(iris)
#'
df_sprintf <- function(dat, fmt){
  dat[] <- lapply(dat, sprintf, fmt = fmt)
  return(dat)
}

#' @export
#' @rdname df_sprintf
df_parenthesis <- function(dat){
  df_sprintf(dat, '(%s)')
}

#' @export
#' @rdname df_sprintf
df_brackets <- function(dat){
  df_sprintf(dat, '[%s]')
}

#' @export
#' @rdname df_sprintf
df_braces <- function(dat){
  df_sprintf(dat, '{%s}')
}
