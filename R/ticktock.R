ticktock_cache <-  new.env()


#' Tick-Tock timer
#'
#' \tabular{ll}{ `tick()` \tab prints the current time and saves it to the
#' hidden vector `.tick` in the global environment \cr `tock()` \tab prints the
#' current time and time difference to `.tick` as set by tick()\cr }
#'
#'
#' @return `tick()` returns [Sys.time()] at the moment the function was called
#'   (invisibly)
#' @export
#'
#' @md
#'
#' @examples
#' tick()
#' # [1] "Tick: 2017-04-02 20:02:59"
#'
#' tock()
#' # [1] "Tock: 2017-04-02 20:03:00 - Diff: 1.4 secs"
#'
#' tock()
#' # [1] "Tock: 2017-04-02 20:03:02 - Diff: 3.2 secs"
#'
tick <- function() {
  .tick <- Sys.time()
  assign('.tick', .tick,  envir = ticktock_cache)
  print(paste('Tick:', .tick))
  invisible(.tick)
}




#' @rdname tick
#' @md
#' @return `tock()` returns a [difftime()] object, containing the time
#'   difference to `.tick` (invisibly).
#' @export
tock <- function() {
  .tick <- get('.tick', envir = ticktock_cache)
  tock_time <- Sys.time()
  tock_diff <- difftime(tock_time, .tick)

  print(sprintf(
    'Tock: %s - Diff: %s',
    tock_time,
    format(
      tock_diff,
      digits = 2))
    )

  invisible(tock_diff)
}
