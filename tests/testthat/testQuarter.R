context("Utils")

testdat <- data.frame(
  a = '2013-Q4',
  b = '2014-Q1',
  c = '2014-Q2',
  d = '2014-Q3',
  e = '2015-Q1',
  f = factor('2015-Q2'),
  stringsAsFactors = FALSE
)

test_that("Greater / Less than operations work for quarters.", {

  a  <- Quarter(testdat$a)
  b  <- Quarter(testdat$b)
  c  <- Quarter(testdat$c)
  c2 <- Quarter(testdat$c)
  d  <- Quarter(testdat$d)
  e  <- Quarter(testdat$e)
  f  <- Quarter(testdat$f)

  expect_true(a < b)
  expect_true(b < c)
  expect_false(c < c2)
  expect_true(c <= c2)
  expect_true(c == c2)
  expect_true(c >= c2)
  expect_true(f > e)
  expect_true(e > d)
  expect_true(d > c2)
})
