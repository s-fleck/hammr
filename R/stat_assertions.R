#' Check if all elements of a vector are valid URS
#'
#' @param x a character vector
#' @export
#'
#' @return True if object is of the desired class
is_urs <- function(x){
  looks_like  <- grepl(rexpat()$valid_urs, x)

  test_cases <- substring(x, 1, 8)
  should     <- substring(x, 9, 9)
  is         <- calc_urs_check_digit(test_cases)

  checksum_ok <- should == is

  return(looks_like & checksum_ok)
}


on_failure(is_urs) <- function(call, env){
  paste("x is not a valid URS")
}


# https://regex101.com/r/bJ5sJ0/1
