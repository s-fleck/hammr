#' Split up a vector into equal sized chunks
#'
#' From: \url{https://gist.github.com/sckott/4632735}
#'
#' @param x An input vector.
#' @param interval The length of the resulting vectors.
#'
#' @family vector tools
#' @export
#' @examples
#' vec <- c("a","b","d","e","f","g","h")
#' vec_chop(vec, 3)
vec_chop <- function(x, interval){
  splt                 <- rep(FALSE, interval)
  splt[1]              <- TRUE
  a <- ceiling(length(x) / length(splt))
  splt  <-  rep(splt, a)
  splt <- cumsum(splt)

  split(x, splt)
}
