#' Get Weekday Component of a date-time (and Monday is the First Day of the Week)
#'
#' A wrapper for [lubridate::wday()] that returns the Weekdays in `ISO 8601`
#' order (Monday is first), while [lubridate] uses the freedom order (Sunday
#' is first).
#'
#' @inheritParams lubridate::wday
#'
#' @return wday2 returns the day of the week as a decimal number
#'   (01-07, Monday is 1) or an ordered factor (Monday is first).
#' @export
#'
#' @examples
wday2 <- function(x, label = FALSE, abbr = TRUE){
  res <- lubridate::wday(x = x, label = label, abbr = abbr)

  if(label){
    sun_name <- grep("sun.*", levels(res), value = TRUE, ignore.case = TRUE)
    res <- forcats::fct_relevel(res, sun_name, after = length(res))
  } else {
    res <- as.integer(res) - 1L
    res[res == 0] <- 7L
  }

  res
}
