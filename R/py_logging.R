###############################################################################
# py_logging.R
#
# Emulate python style logging for four levels: DEBUG, ERROR, INFO, FATAL.
#
# This script wraps the functionality in futile.logger and uses namespaces
# to create different files associated with different levels of debugging.
# The behavior should be entirely equivalent to python logging handlers.
#
# The reason this is needed is that the futile.logging package only supports
# a single "appender" per logger.
#
# Author: Jonathan Callahan
# Source: http://mazamascience.com/WorkingWithData/?p=1727
###############################################################################


#' Python style logging based on futile.logger
#'
#' @param debugLog
#' @param infoLog
#' @param errorLog
#'
#' @return
#' @export
#' @import futile.logger
#' @rdname pylogging
#'
#' @examples
logger.setup <- function(debugLog=NULL, infoLog=NULL, errorLog=NULL) {

  # TODO:  Design behavior when a log file is not passed in.
  if ( is.null(debugLog) ) {
    stop('A debugLog file must be specified.')
  }
  if ( is.null(infoLog) ) {
    stop('A infnoLog file must be specified.')
  }
  if ( is.null(errorLog) ) {
    stop('A errorLog file must be specified.')
  }

  # FATAL messages go to the console
  dummy <- flog.threshold(FATAL)

  # Set up new loggers, one for each log level
  if ( file.exists(debugLog) ) dummy <- file.remove(debugLog)
  dummy <- flog.logger("debug", DEBUG, appender.file(debugLog))

  if ( file.exists(infoLog) ) dummy <- file.remove(infoLog)
  dummy <- flog.logger("info", INFO, appender.file(infoLog))

  if ( file.exists(errorLog) ) dummy <- file.remove(errorLog)
  dummy <- flog.logger("error", ERROR, appender.file(errorLog))

}

# Log at the DEBUG level
#' @export
#' @import futile.logger
#' @rdname pylogging
logger.debug <- function(..., name='ROOT') {
  flog.debug(..., name='debug')
}

# Log at the INFO level
#' @export
#' @import futile.logger
#' @rdname pylogging
logger.info <- function(..., name='ROOT') {
  flog.info(..., name='debug')
  flog.info(..., name='info')
}

# Log at the error level
#' @export
#' @import futile.logger
#' @rdname pylogging
logger.error <- function(..., name='ROOT') {
  flog.error(..., name='debug')
  flog.error(..., name='info')
  flog.error(..., name='error')
}

# Log at the fatal level
#' @export
#' @import futile.logger
#' @rdname pylogging
logger.fatal <- function(..., name='ROOT') {
  flog.fatal(..., name='debug')
  flog.fatal(..., name='info')
  flog.fatal(..., name='error')
  flog.fatal()
}
