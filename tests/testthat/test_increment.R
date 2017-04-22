context("increment")


test_that("increment works as expected", {

  x <- as_date_yq(c(20151, 20164, 20133))

  increment(x, 1)
  increment(x, 5)
  increment(x, -1)
  increment(x, -5)


})
