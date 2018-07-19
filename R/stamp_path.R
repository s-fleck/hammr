#' Title
#'
#' @param x
#' @param stamp
#'
#' @return
#' @export
#'
#' @examples
stamp_path <- function(
  x,
  stamp = as.character(Sys.Date())
){
  stopifnot(is_scalar_character(stamp))
  bp  <- tools::file_path_sans_ext(x)
  ext <- substr(x, nchar(bp) + 1, nchar(x))

  paste0(bp, "_", stamp, ext)
}
