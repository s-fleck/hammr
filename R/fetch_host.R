#' Fetch files from host via FTP
#'
#' @param file filename on host
#' @param outdir local destination directory
#' @param overwrite should existing files be overwritten?
#' @param creds login credentials for host
#'
#' @export
#'
#' @return
fetch_host_file <- function(file, outdir, overwrite = FALSE, creds = creds){
  cat("HOST-Download wird gestartet...\n")

  # Setup paths and credentials

  outfile                     <- file.path(outdir, file)

  if (file.exists(outfile)) {
    if(overwrite){
      warning('Zieldatei existiert und wird ueberschrieben')
    } else {
      stop('Breche ab: Zieldatei existiert und overwrite = FALSE')
    }
  }

  if(missing(creds))    creds <- ui_credentials()
  ftp_commands                <- tempfile()


  # Cleanup files before download
  if (file.exists(ftp_commands))          file.remove(ftp_commands)


  # Fetch file via ftp
  writeLines(paste0("user ", creds$user, "\n", creds$pw, "\n","cd ..","\n","get ", file, "\n","quit"), ftp_commands)
  cmd <- paste0("ftp -n -s:", ftp_commands," mfstat01")
  shell(cmd)
  file.remove(ftp_commands)


  # Move file to destination dir
  copy_ok <- file.copy(file, file.path(outdir, file), overwrite = overwrite)


  if(copy_ok) {
    file.remove(file)
    cat("File saved to: ", outfile)
    invisible(TRUE)
  } else {
    stop('Something went wrong')
  }
}


ui_credentials <- function(){
  res <- list()

  res$user  <- readline("User:")
  res$pw    <- readline("Passwort:")

  return(res)
}


#' Send Querry to Host
#'
#' @param q
#' @param con
#' @export

query_host_db2 <- function(q, con = RODBC::odbcConnect(dsn=gvk_secrets['dsn'],
                                                uid=gvk_secrets['uid'],
                                                pwd=gvk_secrets['pwd'])
               ){

  res        <- RODBC::sqlQuery(con, q, errors=FALSE)
  con_string <- strsplit(attr(con, "connection.string"), ";", fixed = TRUE)[[1L]]

  attr(res, 'date')       <- Sys.time()
  attr(res, 'fetch_date') <- attr(res, 'date')
  attr(res, 'con')        <- c(con_string, table = table)
  class(res)              <- c('host_db2_fetch', class(res))

  RODBC::odbcCloseAll()

  return(res)
}


#' Fetch db2 tables from host
#'
#' @section Tips:
#'
#' Fetch 'sysibm.systables' to get infos about all tables on db2 server (warning: big query)
#'
#' @param table name of the table to be retrieved
#' @param con an ROBC connection object
#'
#' @return
#' @export
#'
#' @examples
fetch_host_db2 <-function(table, con = RODBC::odbcConnect(dsn=gvk_secrets['dsn'],
                                                          uid=gvk_secrets['uid'],
                                                          pwd=gvk_secrets['pwd'])){
  q = paste('select * from', table)
  res <- query_host_db2(q, con)
  attr(res, 'source') <- table

  return(res)
}


host_db2_table_info <-function(table, con = RODBC::odbcConnect(dsn=gvk_secrets['dsn'],
                                                          uid=gvk_secrets['uid'],
                                                          pwd=gvk_secrets['pwd'])){

  q <- "select NAME,TBNAME,COLTYPE,LENGTH,REMARKS,SCALE from sysibm.syscolumns
            where tbcreator = 'SGVP' and tbname='TURPRIV' ;'"

  res <- query_host_db2(q, con)

  return(res)
}
