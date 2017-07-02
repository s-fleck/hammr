context("reporting_period")


test_that("reporting_period works as expected", {
  x <- 1L

  expect_silent(
    reporting_period(x) <- as_date_ym(201612)
  )

  expect_identical(
    reporting_period(x),
    date_ym(2016, 12)
  )

  expect_true(
    has_reporting_period(x)
  )

})
