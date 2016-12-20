credentials <- function(dat){
  class(dat) <- c('Credentials', 'list')
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
ui_creds_db2 <- function(){
  res <- list()

  res$user  <- readline("User:")
  res$pw    <- readline("Passwort:")
  res$dsn   <- readline("dsn:")

  return(res)
}
