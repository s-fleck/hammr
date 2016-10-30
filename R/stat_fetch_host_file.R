#' Fetch files from host via FTP
#'
#' @param file filename on host
#' @param outdir local destination directory
#' @param overwrite should existing files be overwritten?
#' @param creds login credentials for host
#' @param ... params passed to fetch_ftp
#'
#' @export
#'
#' @return
fetch_host_file <- function(file, outdir = '.', creds = ftp_creds, overwrite = FALSE, ...){
  message("HOST-Download wird gestartet...\n")

  fetch_ftp(file, outdir, creds, overwrite, ...)
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
