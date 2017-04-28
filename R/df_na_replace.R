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
df_na_replace <- function(dat, replace, inf = FALSE, as_char = FALSE){
  UseMethod('df_na_replace')
}




#' @export
df_na_replace.data.frame <- function(
  dat,
  replace,
  inf = FALSE,
  as_char = FALSE
){
  # Precodntions
    assert_that(is.flag(inf))


  # Logic
  if(as_char){
    dat <- na_cols_to_character(dat)
  }

    if(!inf){
      dat[is.na(dat)] <- replace
    } else {
      dat[is.na(dat) | is.infinite(dat)] <- replace
    }

  return(dat)
}




#' @export
df_na_replace.data.table <- function(
  dat,
  replace,
  inf = FALSE,
  as_char = FALSE
){
  assert_that(is.flag(inf))
  res <- data.table::copy(dat)

  if(as_char){
    res <- na_cols_to_character(res)
  }

  if(!inf){
    selector <- function(x) which(is.na(x))
  } else {
    selector <- function(x) which(is.na(x) | is.infinite(x))
  }

  for (j in seq_along(res)) {
    data.table::set(res, selector(dat[[j]]), j, replace)
  }

  return(res)
}





#' @export
#' @rdname df_na_replace
df_na0 <- function(dat, inf = FALSE){
  df_na_replace(dat, replace = 0, inf = inf)
}

#' @export
#' @rdname df_na_replace
df_na_blank <- function(dat, inf = FALSE){
  df_na_replace(dat, replace = '', inf = inf, as_char = TRUE)
}



# utils -------------------------------------------------------------------

na_cols_to_character <- function(dat){
  dat[] <- purrr::map_if(dat, anyNA, function(x){
    x[is.nan(x)] <- NA
    as.character(x)
  })
  dat
}

