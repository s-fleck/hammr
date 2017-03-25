#' @export
string_pad <- function(x, width) {
  .Deprecated('Deprecated. use stringi::stri_pad_left() instead.')
  fs <- paste0('%0', width, 'd')

  res <- sprintf(fs, as.integer(x))
  res[grep('NA', fixed = TRUE, res)] <- NA
  return(res)
}
