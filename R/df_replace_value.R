# Replace if --------------------------------------------------------------

#' Replace values in Data Frame columns that match a predicate
#'
#' @param dat a data.frame or data.table
#' @param predicate A predicate function to be applied to the columns of `dat`.
#'   Only those columns where `predicate()` evalutes to `TRUE` will be modified.
#' @param value value to replace
#' @param replace replacement value
#'
#' @return `dat` with `value` replaced by `replace`
#' @inheritParams df_replace_na
#'
#' @export
df_replace_value_if <- function(
  dat,
  predicate,
  value,
  replace
){

  fun <- function(x) {
    if (predicate(x)) {
      ifelse(x == value, replace, x)
    } else {
      x
    }
  }

  for (i in seq_along(dat)) {
    dat[[i]] <- fun(dat[[i]])
  }


  dat
}




#' Replace value in numeric columns
#'
#' @inheritParams df_replace_value_if
#' @param replace replacement value. will be coerced to numeric.
#'
#' @inherit df_replace_value_if return
#'
#' @export
df_replace_value_num <- function(
  dat,
  value,
  replace
){
  df_replace_value_if(
    dat = dat,
    predicate = is.numeric,
    value = value,
    replace = as.numeric(replace)
  )
}




# Replace NA --------------------------------------------------------------

#' Replace NA values
#'
#' Replaces all `NA`s and `NAN`s in a data.frame or data.table with `replace`.
#'
#' @param dat a data.frame or data.table
#' @param as_char logical. if `TRUE`  each column where a value will be
#'   replaced is automatically converted to `character` (useful when dealing
#'   with factors).
#' @inheritParams na_replace
#'
#' @return A data.frame with all `NA`s replaced by `replace`
#' @md
#' @family data.frame tools
#' @export
#'
#' @export
df_replace_na <- function(dat, replace, inf = FALSE, as_char = FALSE){
  UseMethod("df_replace_na")
}




df_na_replace <- function(dat, replace, inf = FALSE, as_char = FALSE){
  .Deprecated("Please use df_replace_na")
  UseMethod("df_replace_na")
}




#' @export
df_replace_na.data.frame <- function(
  dat,
  replace,
  inf = FALSE,
  as_char = FALSE
){
  # Precodntions
  assert_that(is.flag(inf))


  # Logic
  if (as_char) {
    dat <- na_cols_to_character(dat)
  }

  if (!inf) {
    dat[is.na(dat)] <- replace
  } else {
    dat[is.na(dat) | is.infinite(dat)] <- replace
  }

  return(dat)
}




#' @export
df_replace_na.data.table <- function(
  dat,
  replace,
  inf = FALSE,
  as_char = FALSE
){
  assert_that(is.flag(inf))
  res <- data.table::copy(dat)

  if (as_char) {
    res <- na_cols_to_character(res)
  }

  if (!inf) {
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
#' @rdname df_replace_na
df_na0 <- function(dat, inf = FALSE){
  df_replace_na(dat, replace = 0, inf = inf)
}




#' @export
#' @rdname df_replace_na
df_na_blank <- function(dat, inf = FALSE){
  df_replace_na(dat, replace = "", inf = inf, as_char = TRUE)
}




# utils -------------------------------------------------------------------

na_cols_to_character <- function(dat){
  dat[] <- purrr::map_if(dat, anyNA, function(x){
    x[is.nan(x)] <- NA
    as.character(x)
  })
  dat
}
