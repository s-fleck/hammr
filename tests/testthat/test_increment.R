context("increment")


test_that("increment works as expected", {

  x <- as_date_yq(c(14, 101, 212341, 212344, 20151, 20164))

  expect_silent(xp1 <- increment(x, 1))
  expect_silent(xp5 <- increment(x, 5))
  expect_silent(xm1 <- increment(x, -1))
  expect_silent(xm5 <- increment(x, -5))

  expect_identical(xp1, as_date_yq(c(21, 102, 212342, 212351, 20152, 20171)))
  expect_identical(xp5, as_date_yq(c(31, 112, 212352, 212361, 20162, 20181)))
  expect_identical(xm1, as_date_yq(c(13, 94, 212334, 212343, 20144, 20163)))
  expect_identical(xm5, as_date_yq(c(3, 84, 212324, 212333, 20134, 20153)))

})
