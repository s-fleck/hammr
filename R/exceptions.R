cannot_be_sorted_error <- function(text ='') {
  msg <- 'Input data cannot be sorted.' %_% text

  condition(c('cannot_be_sorted_error', 'error'),
            message = msg)
}

line_cannot_be_parsed_error <- function(text ='') {
  msg <- 'Line cannot be parsed.' %_% text

  condition(c('line_cannot_be_parsed_error', 'error'),
            message = msg)
}


field_cannot_be_parsed_error <- function(text ='') {
  msg <- 'Field cannot be parsed: ' %_% text

  condition(c('line_cannot_be_parsed_error', 'error'),
            message = msg)
}


