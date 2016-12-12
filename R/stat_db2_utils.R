#' Check if db2 table exists
#'
#' @param con  an \code{\link{RODBC}} connection object
#' @param tname name of the table
#'
#' @return TRUE/FALSE if table exists
#' @export
#'
#' @examples
db2ExistsTable <- function(con, tname){
  tname <- toupper(tname)
  res   <- RODBC::sqlQuery(con, paste("select * from", tname, "FETCH FIRST 1 ROWS ONLY"))
  return(class(res) %identical% 'data.frame')
}


#' @export
db2ExistsSequence <- function(con, sname){

  sname <- trimws(toupper(sname))
  schema <- stringi::stri_split(sname, fixed = '.')[[1]][1]
  tname  <- stringi::stri_split(sname, fixed = '.')[[1]][2]

  res   <- RODBC::sqlQuery(con,
                           "select *
                           from syscat.sequences")

  any(trimws(as.character(res$SEQSCHEMA)) == schema &
        as.character(res$SEQNAME) == tname)
}

