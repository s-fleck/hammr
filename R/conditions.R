ftp_530_creds_error<- function(text) {
  msg <- 'FTP login error:' %_% text

  condition(c('ftp_530_creds_error', 'error'),
            message = msg)
}

fun_arg_error <- function(text){
  msg <- 'Function argument error:' %_% text

  condition(c('fun_arg_error', 'error'),
            message = msg)
}



#' Condition constructor
#'
#' From http://adv-r.had.co.nz/beyond-exception-handling.html
#'
#' @param subclass
#' @param message
#' @param call
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' # Construct a custom condition
#'    malformed_log_entry_error <- function(text) {
#'      msg <- paste0("Malformed log entry: ", text)
#'      condition(c("malformed_log_entry_entry", "error"),
#'            message = msg,
#'            text = text
#'            )
#'    }
#'
#'
#' # Signal the condition
#'    parse_log_entry <- function(text) {
#'      if (!well_formed_log_entry(text)) {
#'      stop(malformed_log_entry_error(text))
#'    }
#'
#'
#' # Handle the condition
#' tryCatch(
#'   malformed_log_entry = function(e) NULL,
#'   parse_log_entry(text)
#'   )
#'
#'
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}
