#' A unified format for storing Credentials
#'
#' @param dat a `list`
#' @aliases Credentials
#'
#' @export
credentials <- function(dat){
  class(dat) <- c('Credentials', 'list')
  return(dat)
}




#' @rdname credentials
credentials_db <- function(dat){
  class(dat) <- c('Credentials_db', 'Credentials', 'list')
  return(dat)
}




#' Console user input for login credentials
#'
#' Asks for username and password in the console. `ui_credentials_db`
#' additonally asks for an ODBC dsn or JDBC url.
#'
#' @param msg Message to be displayed when asking for credentials
#'
#' @return a list of class `Credentials`
#'
#' @export
ui_credentials <- function(msg = NULL){
  if(!is.null(msg)){
    cat(msg, '\n\n')
  }

  res       <- list()
  res$user  <- readline("User:")
  res$pw    <- readline("Passwort:")

  credentials(res)
}




#' @rdname ui_credentials
#' @export
ui_credentials_db <- function(msg = NULL){
  if(!is.null(msg)){
    cat(msg, '\n\n')
  }

  res <- list()
  res$user    <- readline("User:")
  res$pw      <- readline("Passwort:")
  res$odbcdsn <- readline("ODBC DSN (optional):")
  res$jdcburl <- readline("JDBC URL (optional):")

  credentials_db(res)
}
