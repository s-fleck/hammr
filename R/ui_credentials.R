#' @export
ui_credentials <- function(){
  res <- list()

  res$user  <- readline("User:")
  res$pw    <- readline("Passwort:")

  return(res)
}


#' @export
ui_creds_db2 <- function(){
  res <- list()

  res$user  <- readline("User:")
  res$pw    <- readline("Passwort:")
  res$dsn   <- readline("dsn:")

  return(res)
}
