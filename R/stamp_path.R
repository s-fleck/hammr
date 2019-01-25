#' Add a Stamp to a Path
#'
#' @param x a `character` vector
#' @param stamp an atomic scalar. Before it is added to the output path between
#'   the filename and the file extension it is passed through [base::format()].
#'   The special strings `":date:"`, `":time:"`, `":timestamp:"` can be used for
#'   the current date/time/datetime in ISO format without separators.
#' @param ... passed on to  `base::format()`
#'
#' @return a `character` vector of the same length as `x`
#' @export
#'
stamp_path <- function(
  x,
  stamp = Sys.time(),
  stamp_fmt = {
    if (inherits(stamp, "Date")) "%Y%m%d"
    else if (inherits(stamp, "POSIXt")) "%Y%m%dT%H%M%S"
    else if (dint::is_date_yq(stamp)) "%Yq%q"
    else if (dint::is_date_ym(stamp)) "%Ym%m"
    else if (dint::is_date_yw(stamp)) "%Ym%v"
    else NULL
  },
  ...
){
  switch(
    stamp,
    ":date:" = format(Sys.Date(), "%Y%m%d"),
    ":time:" = format(Sys.time(), "H%M%S"),
    ":timestamp:" = format(Sys.time(), "%Y%m%dT%H%M%S")
  )


  if (!is.null(stamp_fmt)){
    stamp <- format(stamp, format = stamp_fmt)
  } else {
    stamp <- format(stamp, ...)
  }

  stopifnot(is_scalar_atomic(stamp))
  stopifnot(is.character(x))

  bp  <- tools::file_path_sans_ext(x)
  ext <- substr(x, nchar(bp) + 1, nchar(x))

  paste0(bp, "_", stamp, ext)
}
