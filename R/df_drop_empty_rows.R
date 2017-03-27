#' Drop empty or cols from data.frame
#'
#' Based on: http://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
#'
#' @param dat a data.frame
#'
#' @return a data.frame without columns/rows that are only NA
#'
#' @family data.frame tools
#'
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
    with=FALSE
  ]
}




#' @export
df_drop_empty_cols.data.frame <- function(dat){
  dat[, which(vapply(dat, FUN.VALUE = logical(1), identify_empty_cols))]
}

identify_empty_cols <- function(x) !all(is.na(x))




#' @export
#' @rdname drop_emtpy_cols
df_drop_empty_rows <- function(dat){
  dat[rowSums(is.na(dat)) != ncol(dat), ]
}
