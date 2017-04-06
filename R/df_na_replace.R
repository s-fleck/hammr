#' Replace NA values
#'
#' Replaces all `NA`s and `NAN`s in a data.frame or data.table with `replace`.
#'
#' @param dat a data.frame or data.table
#' @param replace value to replace `NAs` with. If `replace` is not `numeric`,
#'   each column where a value will be replaced is automatically converted to
#'   `character`.
#' @param inf logical. if `TRUE`, `inf` values are treated like `NAs`
#'
#' @return A data.frame with all `NA`s replaced by `replace`
#' @md
#' @family data.frame tools
#' @export
#'
#' @export
df_na_replace <- function(dat, replace = '', inf = FALSE){
  UseMethod('df_na_replace')
}




#' @export
df_na_replace.data.frame <- function(dat, replace = '', inf = FALSE){
  assert_that(is.flag(inf))

  res <- dat

  if(!is.numeric(replace)){
    res[] <- purrr::map_if(
      dat,
      function(x) any(is.na(x)),
      function(x) as.character(x)
    )
  }


  res[is.na(dat)] <- replace

  if(inf){
    res[is.infinite(dat)] <- replace
  }

  return(res)
}




#' @export
df_na_replace.data.table <- function(dat, replace = '', inf = FALSE){
  assert_that(is.flag(inf))
  res <- data.table::copy(dat)

  if(!inf){
    selector <- function(x) is.na(x) | is.nan(x)
  } else {
    selector <- function(x) is.na(x) | is.nan(x) | is.infinite(x)
  }


  if(!is.numeric(replace)){
    res[] <- purrr::map_if(
      dat,
      function(x) any(selector(x)),
      function(x) as.character(x)
    )
  }


  for (j in seq_along(res)) {
    data.table::set(res, which(selector(dat[[j]])), j, replace)
  }

  return(res)
}





#' @export
#' @rdname df_na_replace
df_na0 <- function(dat, inf = FALSE){
  df_na_replace(dat, replace = 0, inf = inf)
}
