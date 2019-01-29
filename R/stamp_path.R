#' Add a Stamp to a Path
#'
#' @param x a `character` vector
#' @param stamp an atomic scalar. Before it is added to the output path between
#'   the filename and the file extension it is passed through [base::format()].
#'   The special strings `":date:"`, `":time:"`, `":timestamp:"` can be used for
#'   the current date/time/datetime in ISO format without separators.
#' @param stamp_fmt `character` scalar. Assumes that the `format()` method for
#'   whatever class `stamp` is has a `format` argument (otherwise it is silently
#'   ignored). This is for example true for the `Date`, `POSIXt`, and
#'   [dint][dint::dint-package] dates, for which sensible default formats
#'   are defined.
#' @param ... passed on to  [format()]
#'
#' @return a `character` vector of the same length as `x`
#' @export
#'
#' path <- "path/to/important.xlsx"
#' stamp_path(path)
#' stamp_path(path, ":date:")
#' stamp_path(path, ":time:")
#' stamp_path(path, ":timestamp:")
#' stamp_path(path, "hello")
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
  stamp <- switch(
    stamp,
    ":date:" = format(Sys.Date(), "%Y%m%d"),
    ":time:" = format(Sys.time(), "%H%M%S"),
    ":timestamp:" = format(Sys.time(), "%Y%m%dT%H%M%S"),
    stamp
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
