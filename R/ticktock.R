#' Tick-Tock timer
#'
#' Convenient function to quickly measure time differences
#'
#' @aliases tock
#'
#' @return tick() displays current time and saves it to the hidden vector .tick in the global environment
#' @export
#'
#' @rdname ticktock
#'
#' @examples
#' tick()
#' # [1] "Tick: 2015-10-28 15:45:12"
#' tock()
#' # [1] "Tock: 2015-10-28 15:45:17  - Diff:  4.1 secs"
#'
tick = function() {
  .tick <<- Sys.time()
  print(paste('Tick:', .tick))
}

#' @rdname ticktock
#' @aliases tick
#' @return tock displays current time and time difference to .tick as set by tick()
#' @export

tock = function() {
  print(paste('Tock:', Sys.time(), ' - Diff: ', format(difftime(Sys.time(), .tick), digits=2)))
}


