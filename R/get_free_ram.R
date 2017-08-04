#' Get free phyiscal memory
#'
#' @unit any of `b`, `kb`, `kib`, `mb`, `mib`, `gb`, `gib`
#' @return Available system memory in bytes
#' @export
#'
get_free_ram <- function(unit = "kib"){

  unit <- tolower(unit)

  if(Sys.info()[["sysname"]] == "Windows"){
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- gsub("\r", "", x, fixed = TRUE)
    res <- as.integer(x)

    switch(unit,
      "kib" = res,
      "mib" = res / 1024,
      "gib" = res / 1024^2,
      stop("Unit not valid")
    )


  } else {
    stop("Only supported on Windows OS")
  }
}
