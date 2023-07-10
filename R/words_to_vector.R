#' Turn a string of words (sepparated by spaces) into a vector
#'
#' @param x a `character` vector. defaults to the content of the clipboard.
#' @export
words_to_vector <- function(
  x = read_clipboard()
){
  res <- gsub("\n", " ", x, fixed = TRUE)
  res <- strsplit(res, " ")
  res <- trimws(unlist(res))
  res <- res[Negate(is_blank)(res)]
  unlist(res)
}


read_clipboard <- function(){
  con <- base::file(description='clipboard')
  on.exit(close(con))
  res <- suppressWarnings(readLines(con))
  res
}


#' @export
#' @rdname words_to_vector
global_variable_helper <- function(x = words_to_vector()){
  cat(paste0('"', sort(x), '"', collapse = ",\n"))
}

