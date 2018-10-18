# Compute -----------------------------------------------------------------

#' Calculate with columns of two data.frames of the same shape
#'
#' Takes all columns whose class matches `coltypes` and applies `fun()` to them.
#' This can be used for simple arithmetics such as adding or subtracting (the
#' numeric columns of) two data.frames from each other (see examples). Various
#' preset functions are provided that make use of `df_compute`.
#'
#' @param dat1 a data.frame
#' @param dat2 a data.frame that has the same number of rows and columns, as
#'   well as the same column types and name as `dat1`.
#' @param fun a function that will be executed with each column of `dat1` and
#'   `dat2` whose type matches `coltypes` as first and second argument. The
#'   result of this function must be a vector of length `nrow(dat1) ==
#'   nrow(dat2)`.
#' @param coltypes types of the columns of which to apply fun.
#' @param id_vars A vector of colnames. If provided, ensures that the rows of
#'   both `dat1` and `dat2` are in the correct order. The columns in `id_vars`
#'   cannot contain duplicated values.#'
#' @param ... arguments passed on to fun
#'
#' @rdname df_compute
#'
#' @return a data.frame
#'
#' @family data.frame tools
#' @md
#' @export
#'
#' @examples
#'
#' dat1 <- data.frame(
#'   a = c('alpha', 'beta', 'ceta'),
#'   b = c(10,-10, 90),
#'   c = c(1L, 3L, 5L),
#'   d = factor(c('al', 'dl', 'zl')),
#'   stringsAsFactors = FALSE
#' )
#'
#' dat2 <- data.frame(
#'   a = c('alpha', 'beta', 'ceta'),
#'   b = c(10, 20, 100),
#'   c = c(2L, 1L, 0L),
#'   d = factor(c('bl', 'ul', 'dl')),
#'   stringsAsFactors = FALSE
#' )
#'
#' df_ndiff(dat1, dat2)
#' df_pdiff(dat1, dat2)
#'
#' # df_compute can be passed an arbitrary function
#' df_compute(dat1, dat2, `*`)
#' df_compute(dat1, dat2, fun = function(x, y) paste0(x, '/', y))
#' df_compute(dat1, dat2, fun = function(x, y) paste0(x, '/', y), coltypes = 'factor')
#'
df_compute <- function(
  dat1,
  dat2,
  fun,
  coltypes = 'numeric',
  id_vars = NULL,
  ...
){
  assert_that(is.data.frame(dat1))
  assert_that(is.data.frame(dat2))
  assert_that(identical(nrow(dat1), nrow(dat2)))
  assert_that(identical(ncol(dat1), ncol(dat2)))
  assert_that(identical(names(dat1), names(dat2)))

  assert_that(is.function(fun))
  assert_that(is.character(coltypes))
  assert_valid_id_vars(dat1, dat2, id_vars)
  assert_namespace("dplyr")

  UseMethod('df_compute')
}




#' @export
df_compute.data.table <- function(
  dat1,
  dat2,
  fun,
  coltypes = c('integer', 'numeric'),
  id_vars = NULL,
  ...
){
  d1  <- as.data.frame(dat1)
  d2  <- as.data.frame(dat2)

  res <- df_compute(
    d1, d2,
    fun = fun,
    coltypes = coltypes,
    id_vars = id_vars,
    ...
  )

  as.data.table(res)
}




#' @export
df_compute.data.frame <- function(
  dat1,
  dat2,
  fun,
  coltypes = c('integer', 'numeric'),
  id_vars = NULL,
  ...
){

  # Order columns if id_vars is supplied
  if(!is.null(id_vars)){
    dat1 <- merge(dat1, dat2[, id_vars], by = id_vars, all = TRUE)
    dat2 <- merge(dat2, dat1[, id_vars], by = id_vars, all = TRUE)
  }

  # identify numeric columns
    select_columns <- function(x) class(x) %in% coltypes

    numcols     <- which(vapply(dat1, select_columns, logical(1)))
    numcols_chk <- which(vapply(dat2, select_columns, logical(1)))
    assert_that(identical(numcols, numcols_chk))

  # Apply the function columnwise
    res <- dat1
    for(i in numcols){
      value <- fun(dat1[[i]], dat2[[i]], ...)
      assert_that(
        identical(length(value), nrow(dat1)) &&
        is.atomic(value)
      )
      res[[i]] <- value
    }

  return(res)
}




assert_valid_id_vars <- function(dat1, dat2, id_vars){
  if(is.null(id_vars)) return(TRUE)

  assert_that(is.character(id_vars))
  assert_that(all(id_vars %in% names(dat1)))

  for(var in id_vars){
    assert_that(
      all_are_distinct(dat1[[var]]) &&
      all_are_distinct(dat2[[var]])
    )
  }

  x1 <- dat1 %>%
    dplyr::select_(.dots = id_vars) %>%
    dplyr::arrange_(.dots = id_vars)

  x2 <- dat2 %>%
    dplyr::select_(.dots = id_vars) %>%
    dplyr::arrange_(.dots = id_vars)

  assert_that(identical(x1, x2))
}



# Utils (exported) --------------------------------------------------------

#' `df_diff`: subtracts numeric and integer columns
#'
#' @md
#' @rdname df_compute
#' @export
df_ndiff <- function(dat1, dat2, coltypes = c('integer', 'numeric'), ...){
  df_compute(dat1, dat2, fun = `-`, coltypes = coltypes, ...)
}




#' `df_pdiff`: computes fractional difference between numeric and integer
#'  columns with the following formular: `(dat1-dat2)/dat2`.
#'
#' @param percent `df_pdiff` only: return difference in percent rather than
#'   as a fraction.
#'
#' @md
#' @rdname df_compute
#' @export
df_pdiff <- function(
  dat1,
  dat2,
  coltypes = c('integer', 'numeric'),
  percent = FALSE,
  ...
){
  df_compute(
    dat1, dat2,
    fun = pdiff,
    coltypes = coltypes,
    percent = percent,
    ...
  )
}




#' `df_add`: Add numeric/integer columns
#'
#' @md
#' @rdname df_compute
#' @export
df_add <- function(
  dat1,
  dat2,
  coltypes = c('integer', 'numeric'),
  ...
){
  df_compute(dat1, dat2, fun = `+`, coltypes = coltypes)
}

# Utils -------------------------------------------------------------------

pdiff <- function(x, y, percent){
  res <- (x - y) / y
  if(percent) {
    return(res * 100)
  } else{
    return(res)
  }
}
