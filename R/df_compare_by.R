df_compare_by <- function(dat1, dat2, fun, coltypes, ...){
  UseMethod('df_compare_by')
}




#' @export
df_compare_by.data.frame <- function(
  dat1,
  dat2,
  by,
  fun,
  coltypes = 'numeric',
  ...
){
  d1 <- merge(dat1, dat2[, by], by = by, all = TRUE)
  d2 <- merge(dat2, dat1[, by], by = by, all = TRUE)

  res <- df_compare(d1, d2, fun = fun, coltypes = coltypes, ...)

  as.data.table(res)
}
