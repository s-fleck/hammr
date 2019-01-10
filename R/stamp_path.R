#' Add a Stamp to a Path
#'
#' @param x a `character` vector
#' @param stamp an atomic scalar. Before it is added to the output path between
#'   the filename and the file extension it is passed through [base::format()].
#'   The special strings `":date:"`, `":time:`, `":timestamp:"` can be used for
#'   the current date/time/datetime in ISO format without separators.
#' @param ... passed on to  `base::format()`
#'
#' @return a `character` vector of the same length as `x`
#' @export
#'
stamp_path <- function(
  x,
  stamp = format(Sys.time(), "%Y%m%dT%H%M%S"),
  ...
){
  switch(
    stamp,
    ":date:" = format(Sys.date(), "%Y%m%d"),
    ":time:" = format(Sys.time(), "H%M%S"),
    ":timestamp:" = format(Sys.time(), "%Y%m%dT%H%M%S")
  )


  stopifnot(is_scalar_atomic(stamp))
  stopifnot(is.character(x))
  stamp <- format(stamp, ...)
  bp  <- tools::file_path_sans_ext(x)
  ext <- substr(x, nchar(bp) + 1, nchar(x))

  paste0(bp, "_", stamp, ext)
}
