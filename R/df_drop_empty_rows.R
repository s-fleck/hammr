#' Title
#'
#' http://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
df_drop_empty_cols <- function(dat){
  UseMethod('df_drop_empty_cols')
}




#' @export
df_drop_empty_cols.data.table <- function(dat){
  dat[,
     which(vapply(
       dat, FUN.VALUE = logical(1), identify_empty_cols)),
     with=FALSE
     ]
}




#' @export
df_drop_empty_cols.data.frame <- function(dat){
  dat[, which(vapply(dat, FUN.VALUE = logical(1), identify_empty_cols))]
}




identify_empty_cols <- function(x){
  !all(is.na(x))
}


#' Title
#'
#' http://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
df_drop_empty_rows <- function(dat){
  dat[rowSums(is.na(dat)) != ncol(dat), ]
}



