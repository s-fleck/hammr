credentials <- function(dat){
  class(dat) <- c('Credentials', 'list')
  return(dat)
}


credentials_db <- function(dat){
  class(dat) <- c('Credentials_db', 'Credentials', 'list')
  return(dat)
}


#' Console user input for login credentials
#'
#' @param msg Message to be displayed when asking for credentials
#'
#' @export
ui_credentials <- function(msg = NULL){
  res <- list()


  if(!is.null(msg)){
    cat(msg, '\n\n')
  }

  res$user  <- readline("User:")
  res$pw    <- readline("Passwort:")

  credentials(res)
}

#' @rdname ui_credentials
#' @export
ui_credentials_db <- function(msg = NULL){
  res <- list()


  if(!is.null(msg)){
    cat(msg, '\n\n')
  }

  res$user    <- readline("User:")
  res$pw      <- readline("Passwort:")
  res$odbcdsn <- readline("ODBC DSN (optional):")
  res$jdcburl <- readline("JDBC URL (optional):")

  credentials_db(res)
}
