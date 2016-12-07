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
