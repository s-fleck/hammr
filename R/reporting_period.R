reporting_period <- function(x){
  attr(x, 'reporting_period')
}




`reporting_period<-` <- function(x, value){
  assert_that(inherits(value, 'date_xx'))
  attr(x, 'reporting_period') <- value
  x
}




has_reporting_period <- function(x){
  is_date_xx(reporting_period(x))
}
