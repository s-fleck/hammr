#' Check if object is of a certain class
#'
#' @param dat the object
#' @param class the class to be checked for
#'
#' @return True if object is of the desired class
#' @export
#'
#' @examples
#'
#' x = data.frame()
#' is.class(x, 'data.frame')
is_class <- function(dat, class){
  class %in% class(dat)
}

assertthat::on_failure(is_class) <- function(call, env){
  class = eval(call$class)
  paste("Requires an object of class", class, "as input")
}



has_field <- function(dat, field, type){
  field_exists           <- field %in% names(dat)
  field_has_correct_type <- ifelse(class(dat[[field]]) == type, TRUE, FALSE)
  return(field_exists && field_has_correct_type)
}


#' Check if any of the classes of the object match a certain string
#'
#' @param dat the object
#' @param choices  the class to be checked for
#'
#' @return True if any of the object classes are the desired class
#' @export
#'
#' @examples
#'
#' x = data.frame()
#' class(x) <- c('data.frame', 'test')
#' is_class(x, 'test')
#'
is_any_class <- function(dat, choices){
  any(choices %in% class(dat))
}

assertthat::on_failure(is_any_class) <- function(call, env){
  choices = paste(eval(call$choices), collapse=", ")
  paste("Input must be an object of any of the following classes:", choices)
}



