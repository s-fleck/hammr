#' Condition constructor
#'
#' A constructur function for conditions, taken from
#' \url{http://adv-r.had.co.nz/beyond-exception-handling.html}
#'
#' @param subclass Subclass to assign to the condition
#' @param message  message to be passed to the condition
#' @param call     call passed on to the conditon
#' @param ...      further list elements to be passed on to the resulting object
#'
#' @md
#' @return a condition object
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Construct a custom condition
#' malformed_log_entry_error <- function(text) {
#'   msg <- paste0("Malformed log entry: ", text)
#'   condition(
#'     c("malformed_log_entry_entry", "error"),
#'     message = msg,
#'     text = text
#'   )
#' }
#'
#'
#' # Signal the condition
#' parse_log_entry <- function(text) {
#'   if (!well_formed_log_entry(text)) {
#'     stop(malformed_log_entry_error(text))
#'    }
#' }
#'
#'
#' # Handle the condition
#' tryCatch(
#'   malformed_log_entry = function(e) NULL,
#'   parse_log_entry(text)
#' )
#' }
#'
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}



error <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "error", 'condition'),
    list(message = message, call = call, ...)
  )
}



fun_arg_error <- function(text){
  msg <- paste('Function argument error:', text)

  condition(
    c('fun_arg_error', 'error'),
    message = msg
  )
}
