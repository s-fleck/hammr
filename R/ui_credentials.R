#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
credentials <- function(dat){
  class(dat) <- c('Credentials', 'list')
  return(dat)
}


credentials_db <- function(dat){
  class(dat) <- c('Credentials_db', 'Credentials', 'list')
  return(dat)
}


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
