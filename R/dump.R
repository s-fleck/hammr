#' Conveniently transfer R-data between sessions
#'
#' @param x
#' @param dump_file
#'
#' @return
#' @export
#'
#' @examples
beam <- function(
  x,
  dump_file = file.path(path.expand("~"), ".hammr-beam-buffer.rds")
){
  if (missing(x)){
    return(beam_down(dump_file, cleanup = TRUE))
  }

  beam_up(x, dump_file = dump_file)
}



#' @export
#' @rdname beam
beam_up <- function(
  x,
  dump_file = file.path(path.expand("~"), ".hammr-beam-buffer.rds")
){
  saveRDS(x, dump_file)
  cat(sprintf("Stored `%s` (%s) in pattern buffer", deparse(substitute(x)), human_mem(file.size(dump_file))))
  invisible(x)
}




#' @export
#' @rdname beam
beam_down <- function(
  dump_file = file.path(path.expand("~"), ".hammr-beam-buffer.rds"),
  cleanup = true
){
  print(dump_file)
  res <- readRDS(dump_file)

  cat(sprintf("Beamed down `%s` (%s)", deparse(substitute(x)), human_mem(file.size(dump_file))))
  if (cleanup){
    unlink(dump_file)
  } else {
    cat(" - note: buffer not removed: ", dump_file)
  }

  res
}
