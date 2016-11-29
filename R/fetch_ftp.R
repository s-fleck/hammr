#' @export
fetch_ftp <- function(.file, .outdir = '.', .creds = NULL, .server, .overwrite = FALSE, .mode = 'ascii', ...) {

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


  ftp_comfile  <- generate_ftp_command_file('cmds', creds = .creds, mode = .mode, files = .file, local_dir = .outdir)
  cmd          <- paste0("ftp -n -s:", ftp_comfile, " ", .server)
  ftplog       <- shell(cmd, intern = TRUE)
  file.remove(ftp_comfile)


  message('FTP Log\n',
          '---------------------------------------------------------------------\n',
          paste(ftplog, collapse = '\n'), '\n',
          '---------------------------------------------------------------------\n')

  res <- tryCatch(check_ftp_log(ftplog),
                  'ftp_530_creds_error' = function(x) {
                    retry <- tolower(readline('Retry? (y/n): ')) %in%  c('y', 'yes')

                    if (retry) {
                      fetch_ftp(
                        .file      = .file,
                        .outdir    = .outdir,
                        .creds     = NULL,
                        .server    = .server,
                        .overwrite = .overwrite,
                        .mode      = .mode
                      )
                    } else {
                      stop(x)
                    }
                  })


  # Move file to destination dir
  tfile   <- file.path(.outdir , .file)
  copy_ok <- file.exists(outfile)

  message('Successfully transferred ',   sum(copy_ok), ' of ', length(.file), ' files.\n')

  if(.outdir == '.'){
    .outdir <- getwd()
  }

  if(all(copy_ok)) {
    message("File(s) saved to: ", .outdir, '\n')
    invisible(TRUE)
  } else if (sum(copy_ok > 0)){
    message("File(s) saved to: ", .outdir, '\n')
    stop('Not all files were transferred successfully')
  } else {
    stop('Something went wrong. No files were transferred successfully')
  }

}


generate_ftp_command_file <- function(fname, creds, mode, files, local_dir, get_command = 'get'){

  if (file.exists(fname)){
    file.remove(fname)
  }


  # Create ftp command file
  logon  <- paste0("user ", creds$user, "\n", creds$pw, "\n", "cd .. \n", "lcd ", '"', local_dir, '"', "\n")
  mode   <- paste(mode, '\n')
  gets   <- paste(get_command, files, collapse = '\n')
  logoff <- "\nquit"

  coms <- paste0(logon, mode, gets, logoff)
  writeLines(coms, fname)
  return(fname)
}


check_ftp_log <- function(dat){
  msg530 <- grep('^530', dat, value = TRUE)

  if(length(msg530) > 0){
    msg <- paste(msg530, collapse = '\n')
    msg <- gsub('\r', '', msg)
    stop(ftp_530_creds_error(msg))
  }
}
