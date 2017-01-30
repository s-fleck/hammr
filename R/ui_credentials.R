credentials <- function(dat){
  class(dat) <- c('Credentials', 'list')
  return(dat)
}


credentials_db <- function(dat){
  class(dat) <- c('Credentials_db', 'Credentials', 'list')
  return(dat)
}


#' @export
ui_credentials <- function(){
  res <- list()

  res$user  <- readline("User:")
  res$pw    <- readline("Passwort:")

  credentials(res)
}


#' @export
ui_credentials_db <- function(){
  res <- list()

  res$user    <- readline("User:")
  res$pw      <- readline("Passwort:")
  res$odbcdsn <- readline("ODBC DSN (optional):")
  res$jdcburl <- readline("JDBC URL (optional):")

  credentials_db(res)
}
