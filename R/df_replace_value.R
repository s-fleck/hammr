# Replace if --------------------------------------------------------------

#' Replace values in Data Frame columns that match a predicate
#'
#' @param dat a `data.frame` or `data.table`
#' @param predicate A predicate function to be applied to the columns of `dat`.
#'   Only those columns where `predicate()` evalutes to `TRUE` will be modified.
#' @param value value to replace
#' @param replace replacement value
#'
#' @return `dat` with `value` replaced by `replace`
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
#' @param as_char logical. if `TRUE`  each column where a value will be
#'   replaced is automatically converted to `character` (useful when dealing
#'   with factors).
#' @param inf logical. if `TRUE` also replaces `Inf` values
#' @param replace_na_string logical. if `TRUE` also replaces `"NA"` strings
#'
#' @return A `data.frame` with all `NA`s replaced by `replace`
#' @inheritParams df_replace_value_if
#' @family data.frame tools
#' @export
#'
#' @examples
#' x <- data.frame(
#'   ok  = c(1, 2, 3),
#'   NAs = c(NA, Inf, NaN),
#'   NAs2 = c("NA", "NA", "dog"),
#'   stringsAsFactors = FALSE
#' )
#'
#' df_replace_na(x, "*")
#' df_replace_na(x, "*", inf = TRUE)
#' df_replace_na(x, "*", replace_na_string = TRUE)
#'
df_replace_na <- function(
  dat,
  replace,
  inf = FALSE,
  as_char = FALSE,
  replace_na_string = FALSE
){
  assert_that(is.scalar(replace))
  assert_that(is_scalar_logical(inf))
  assert_that(is_scalar_logical(as_char))
  assert_that(is_scalar_logical(replace_na_string))

  UseMethod("df_replace_na")
}





#' @export
#' @rdname df_replace_na
df_replace_na.data.frame <- function(
  dat,
  replace,
  inf = FALSE,
  as_char = FALSE,
  replace_na_string = FALSE
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
    dat[is.na(dat) | vapply(dat, is.infinite, c(FALSE, FALSE, FALSE))] <- replace
  }

  if(replace_na_string){
    dat[trimws(as.matrix(dat)) == "NA"] <- replace
  }

  return(dat)
}




#' @export
df_replace_na.data.table <- function(
  dat,
  replace,
  inf = FALSE,
  as_char = FALSE,
  replace_na_string = FALSE
){
  assert_that(is.flag(inf))
  res <- data.table::copy(dat)

  string_replace <- function(x){
    if(replace_na_string){
      trimws(x) == "NA"
    } else {
      FALSE
    }
  }


  if (as_char) {
    res <- na_cols_to_character(res)
  }

  if (!inf) {
    selector <- function(x) which(is.na(x) | string_replace(x) )
  } else {
    selector <- function(x) which(is.na(x) | is.infinite(x) | string_replace(x))
  }

  for (j in seq_along(res)) {
    tryCatch(
      data.table::set(res, selector(dat[[j]]), j, replace),
      error = function(e){
        if (grepl("outside the level.* range", e$message)){
          warning(warningCondition("invalid factor level", class = "InvalidFactorLevelWarning"))
        } else{
          stop(e)
        }
      }
    )
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
  assert_namespace("purrr")

  dat[] <- purrr::map_if(dat, anyNA, function(x){
    x[is.nan(x)] <- NA
    as.character(x)
  })
  dat
}
