#' Check if all elements of a vector are valid URS
#'
#' @param x a character vector
#' @export
#'
#' @return True if object is of the desired class
is_urs <- function(x){
  warning('überprüft prüfziffer nicht')
  all(grepl(rexpat()$valid_urs, x))
}

assertthat::on_failure(is_class) <- function(call, env){
  class = eval(call$class)
  paste("Requires an object of class", class, "as input")
}


# https://regex101.com/r/bJ5sJ0/1
