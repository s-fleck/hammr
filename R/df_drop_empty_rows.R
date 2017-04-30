#' Drop rows or empty or collumns from data.frame
#'
#' Drop columns or rows from a [data.frame] or [data.table] if they contain
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
df_drop_empty_cols <- function(dat){
  assert_that(is.data.frame(dat))
  UseMethod('df_drop_empty_cols')
}




#' @export
df_drop_empty_cols.data.table <- function(dat){
  dat[,
    which(vapply(dat, FUN.VALUE = logical(1), identify_empty_cols)),
    with = FALSE
  ]
}




#' @export
df_drop_empty_cols.data.frame <- function(dat){
  dat[, which(vapply(dat, FUN.VALUE = logical(1), identify_empty_cols))]
}




#' @export
#' @rdname df_drop_empty_cols
df_drop_empty_rows <- function(dat){
  dat[rowSums(is.na(dat)) != ncol(dat), ]
}




# Utils -------------------------------------------------------------------
identify_empty_cols <- function(x) !all(is.na(x))
