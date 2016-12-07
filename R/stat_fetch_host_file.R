#' Fetch files from host via FTP
#'
#' @param file filename on host
#' @param outdir local destination directory
#' @param overwrite should existing files be overwritten?
#' @param creds login credentials for host
#' @param ... params passed to download_ftp
#'
#' @export
#'
#' @return
download_host_file <- function(file, outdir = '.', creds = ftp_creds, server = "mfstat01", overwrite = FALSE, ...){
  message("HOST-Download wird gestartet...\n")

  download_ftp(.file = file, .outdir = outdir, .creds = creds, .server = server, .overwrite = overwrite, ...)
}


#' @export
fetch_host_fwf <- function(file, ...) {
  UseMethod('fetch_host_fwf')
}


#' @export
fetch_host_fwf.character <- function(infile, col_positions, ...){

  if(length(infile) %identical% 1L){
    assert_that(names(col_positions) %identical% c("begin", "end", "col_names"))
  } else {
    assert_that(
      (names(col_positions) %identical% c("begin", "end", "col_names")) ||
      (length(infile) %identical% length(col_positions))
      )
  }

  tdir <- tempdir()


  download_host_file(infile, outdir = tdir, creds = creds, overwrite = TRUE)

  res <- foreach(f = infile, .combine = rbind) %do% {
    tfile <- file.path(tdir, f)

    if(file.exists(tfile)){
      suppressWarnings(
        res   <- readr::read_fwf(tfile,
                                 col_positions = col_positions,
                                 ...)
      )
    } else {
      warning('Skipped ', f, ' (file not found)')
      return(NULL)
    }

    res   <- data.table::as.data.table(res)
    file.remove(tfile)
    return(res)
  }

}






#' Title
#'
#' @param year
#' @param quarter
#' @param mask
#' @param reverse
#'
#' @return
#' @export
#'
#' @examples
generate_yq_filename <- function(year, quarter = NULL, mask = 'example_%s-Q%s', reverse = FALSE){
  if(is.null(quarter)){
    timeframe <- expand.grid(year, c(1:4))
  } else {
    assert_that(length(year) %identical% length(quarter))

    timeframe <- data.frame(
      Var1 = year,
      Var2 = quarter
    )
  }

  if(reverse){
    names(timeframe) <- rev(names(timeframe))
  }

  # Filenames
  res <- foreach(i = seq_len(nrow(timeframe))) %do% {
    sprintf(mask,
            timeframe$Var1[[i]],
            timeframe$Var2[[i]])
  }

  return(unlist(res))
}


fetch_ric_raw_internal <- function(params, creds){
  tdir <- tempdir()


  tryCatch(download_host_file(params$infile, outdir = tdir, creds = creds, overwrite = TRUE),
           'ftp_not_all_files_transferred_error' = function(x) (warning(x)))


  res <- foreach(f = params$infile, .combine = rbind) %do% {
    tfile <- file.path(tdir, f)

    if(file.exists(tfile)){
      suppressWarnings(
        res   <- readr::read_fwf(tfile,
                                 col_positions = params$col_widths,
                                 locale = readr::locale(encoding = 'ISO-8859-1'))
      )
    } else {
      warning('Skipped ', f, ' (file not found)')
      return(NULL)
    }

    res   <- data.table::as.data.table(res)
    file.remove(tfile)
    return(res)
  }
}
