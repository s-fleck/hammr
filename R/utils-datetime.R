#' Get Year, Quarter or Month of a POSIXt object
#'
#' @param x A POSIXt object
#'
#' @rdname getdate
#'
#' @return A character repressentation of the year of x
#' @export
#'
#' @examples
#'
#' get_year(Sys.time())
get_year <- function(x) {
  x %>%
    format(format="%Y") %>%
    as.character() %>%
    `attr<-`('class', c('Year', 'character'))

}


#' @rdname getdate
#' @export
get_quarter <- function(x) {
  res <- x %>%
    get_month %>%
    as.data.frame %>%
    tidyr::separate('.', '-', into = c('y', 'm')) %>%
    dplyr::mutate(m = as.integer(m))

  res$q <- 1
  res$q[res$m >= 4] <- 2
  res$q[res$m >= 7] <- 3
  res$q[res$m >= 10] <- 4

  return(quarter(res$y, res$q))
}


#' @rdname getdate
#' @export
get_month <- function(x) {
  x %>%
    format(format="%Y-%m") %>%
    as.character() %>%
    `attr<-`('class', c('Month', 'character'))
}

#' @rdname getdate
#' @export
get_month_first <- function(x) {
  x %>%
    timeFirstDayInMonth() %>%
    as.character()
}



increment.Month <- function(months, increment = 1L){
  assert_that(all(looks_like_year_month(months)) ||
                all(looks_like_date(months)))

  for(i in 1:length(months)){
    months[i] <- increment_month_internal(months[i], increment)
  }

  return(months)
}




increment_month_internal = function(x, increment = 1L){
  #assert_that(is_class(x, 'BankMonth'))

  year = substr(x, 1, 4) %>%
    as.numeric

  month = substr(x, nchar(x)-1, nchar(x)) %>%
    as.numeric

  month = month + increment

  while(month > 12L){
    month = month - 12
    year  = year + 1
  }

  while(month < -12L){
    month = month + 12
    year  = year - 1
  }

  if(month == 0L){
    month = 12L
    year = year - 1L
  }

  if(month < 0){
    month = 12L + month
    year =  year - 1L
  }

  res = paste(year, formatC(month, format = 'd', width = 2, flag = '0'), sep = '-')
  class(res) = c('BankMonth', 'character')

  return(res)
}
