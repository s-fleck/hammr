context('looks_like_integer')

testdat <- data.frame(
  k = c('1', '1.5', '0.000000001', '100000000000', '99.1'),
  l = factor(c('1', '1.5', '0.000000001', '100000000000', '99.1')),
  m = factor(c('a', NA, NA, NA, NA)),
  stringsAsFactors = FALSE
)

test_that("looks_like_integer works.", {
  a <- looks_like_integer(testdat$k)
  b <- looks_like_integer(testdat$l)
  c <- looks_like_integer(as.numeric(testdat$k))
  d <- looks_like_integer(as.numeric(testdat$m))

  expect_identical(a, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(a, b)
  expect_identical(a, c)
})
