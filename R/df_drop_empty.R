#' Drop empty rows and/or columns from data.frame
#'
#' Drop columns and/or rows from a [data.frame] or [data.table] if they contain
#' only `NA` values.
#'
#' Based on: \url{http://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na}
#'
#' @param dat a data.frame
#'
#' @return a data.frame without columns/rows that are only `NA`
#'
#' @md
#' @family data.frame tools
#' @export
#'
df_drop_empty <- function(dat){
  dat %>%
    df_drop_empty_cols() %>%
    df_drop_empty_rows()
}



# drop cols ---------------------------------------------------------------

#' @export
#' @rdname df_drop_empty
df_drop_empty_cols <- function(dat){
  assert_that(is.data.frame(dat))
  UseMethod('df_drop_empty_cols')
}




#' @export
df_drop_empty_cols.data.table <- function(dat){
  dat[,
    which(vapply(dat, FUN.VALUE = logical(1), identify_nonempty_cols)),
    with = FALSE
  ]
}




#' @export
df_drop_empty_cols.data.frame <- function(dat){
  dat[, which(vapply(dat, FUN.VALUE = logical(1), identify_nonempty_cols))]
}




# drop rows ---------------------------------------------------------------

#' @param drop_blanks treat blank character cells like `NAs`
#'
#' @export
#' @rdname df_drop_empty
df_drop_empty_rows <- function(dat, drop_blanks = FALSE){

  if (drop_blanks){
    m <- as.matrix(dat)
    if (!is.character(m)){
      d <- dims(m)
      m <- matrix(as.character(m))
      dims(m) <- d
    }

    m[m == ""] <- NA_character_
  } else {
    m <- dat
  }


  dat[rowSums(is.na(m)) != ncol(dat), ]
}




# Utils -------------------------------------------------------------------
identify_nonempty_cols <- function(x) !all(is.na(x))
