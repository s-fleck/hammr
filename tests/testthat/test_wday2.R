context("wday2")


test_that("wday2 works as expected", {

  x <- seq(from = as.Date("2016-01-01"), to = as.Date("2016-01-12"), by = "day")

  expect_identical(
    as.character(lubridate::wday(x, label = TRUE)),
    as.character(wday2(x, label = TRUE))
  )

  expect_identical(
    as.character(lubridate::wday(x, label = TRUE, abbr = TRUE)),
    as.character(wday2(x, label = TRUE, abbr = TRUE))
  )

  expect_identical(
    wday2(x),
    as.integer(wday2(x), label = TRUE)
  )

  expect_identical(
    wday2(x),
    as.integer(wday2(x), label = TRUE, abbr = TRUE)
  )
})
