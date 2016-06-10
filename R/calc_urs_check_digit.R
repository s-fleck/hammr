calc_urs_check_digit <- function(x){
  assert_that(is_class(x, 'character'))

  res <- character()

  for(i in x){
    res <- c(res, calc_urs_check_digit_single(i))
  }

  return(res)
}


calc_urs_check_digit_single <- function(x){
  x <- toupper(x)

  M36 = 36L
  M37 = 37L

  z = M36

  for(i in 1:nchar(x)){
    c <- substring(x, i, i) %>%
      charToRaw() %>%
      as.integer()

    zero  <- as.integer(charToRaw('0'))
    nine  <- as.integer(charToRaw('9'))
    alpha <- as.integer(charToRaw('A'))

    if(c >= zero && c <= nine){
      z <- z + c - zero
    } else {
      z <- z + c - alpha + 10
    }

    if(z > M36){
      z = z - M36
    }

    if(z > 0){
      z = 2*z
    }


    if (z > M36) {
      z = z - M37
    }
  }

  z = M37 - z

  if (M36 == z) {
    z = 0
  }

  if (z >= 0 && z <= 9) {
    res <- as.character(z)
  } else{
    res <- z + alpha - 10
    res <- rawToChar(as.raw(res))
  }

  return(res)
}
