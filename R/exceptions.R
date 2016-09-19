cannot_be_sorted_error <- function(text) {
  msg <- 'Input data cannot be sorted.' %_% text

  condition(c('cannot_be_sorted_error', 'error'),
            message = msg)
}

