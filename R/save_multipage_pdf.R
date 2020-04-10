#' Save a list of plots
#'
#' @param x a `list` of [ggplot2::ggplot()] objects or anything supported by
#'   [grid::grid.draw]
#' @param filename `scalar` character, a file system path. Filename should end
#'   in `'.pdf'`.
#' @param width,height `numeric` scalars, Dimension (in `inch`)
#' @param overwrite `logical` scalar. overwrite target file.
#'
#' @note
#'
#' \describe{
#'   \item{A4}{`width =  8.27, height = 11.69`}
#'   \item{A3}{`width = 11.69, height = 16.53`}
#' }
#'
#' @return `filename` (invisibly)
#' @export
save_multipage_pdf = function(
  x,
  filename,
  width,
  height,
  overwrite = FALSE
){
  if (file.exists(filename)){
    if (overwrite)
      unlink(filename)
    else
      stop("File '", filename, "' exists and overwrite == FALSE")
  }

  grDevices::pdf(filename, onefile = TRUE, width = width, height = height)
  on.exit(grDevices::dev.off())

  for (i in seq_along(x)){
    grid::grid.draw(x[[i]])
    if (i < length(x)){
      grid::grid.newpage()
    }
  }

  invisible(filename)
}
