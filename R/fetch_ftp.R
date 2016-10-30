#' @export
fetch_ftp <- function(.file, .outdir = '.', .creds = NULL, .overwrite = FALSE, .mode = 'ascii') {

  # Setup paths and credentials ----
  outfile                     <- file.path(.outdir, .file)

  if (any(file.exists(outfile))) {
    if(.overwrite){
      warning('Mindestens eine Zieldatei existiert und wird ueberschrieben')
    } else {
      stop('Breche ab: Zieldatei existiert und overwrite = FALSE')
    }
  }

  if(is.null(.creds)) {
    .creds <- ui_credentials()
  }


  ftp_commands <- tempfile()
  if (file.exists(ftp_commands)){
    file.remove(ftp_commands)
  }

  # Create ftp command file
  logon  <- paste0("user ", .creds$user, "\n", .creds$pw, "\n", "cd .. \n")
  mode   <- paste(.mode, '\n')
  gets   <- paste('get', .file, collapse = '\n')
  logoff <- "\nquit"
  coms <- paste0(logon, mode, gets, logoff)
  writeLines(coms, ftp_commands)


  # Fetch file via ftp
  cmd    <- paste0("ftp -n -s:", ftp_commands," mfstat01")
  ftplog <- shell(cmd, intern = TRUE)

  message('FTP Log\n',
          '---------------------------------------------------------------------\n',
          paste(ftplog, collapse = '\n'), '\n',
          '---------------------------------------------------------------------\n')
  file.remove(ftp_commands)


  res <- tryCatch(check_ftp_log(ftplog),
                  'ftp_530_creds_error' = function(x) {
                    retry <- tolower(readline('Retry? (y/n): ')) %in%  c('y', 'yes')

                    if (retry) {
                      fetch_ftp(
                        .file      = .file,
                        .outdir    = .outdir,
                        .creds     = NULL,
                        .overwrite = .overwrite
                      )
                    } else {
                      stop(x)
                    }
                  })


  # Move file to destination dir
  copy_ok <- file.copy(from = .file, to = outfile, overwrite = .overwrite)

  message('Successfully transferred ',   sum(copy_ok), ' of ', length(.file), ' files.\n')

  if(all(copy_ok)) {
    file.remove(.file)
    message("File(s) saved to: ", .outdir, '\n')
    invisible(TRUE)
  } else if (sum(copy_ok > 0)){
    file.remove(.file)
    message("File(s) saved to: ", .outdir, '\n')
    stop('Not all files were transferred successfully')
  } else {
    stop('Something went wrong. No files were transferred successfully')
  }

}


check_ftp_log <- function(dat){
  msg530 <- grep('^530', dat, value = TRUE)

  if(length(msg530) > 0){
    msg <- paste(msg530, collapse = '\n')
    msg <- gsub('\r', '', msg)
    stop(ftp_530_creds_error(msg))
  }
}


ui_credentials <- function(){
  res <- list()

  res$user  <- readline("User:")
  res$pw    <- readline("Passwort:")

  return(res)
}
