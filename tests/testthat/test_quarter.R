context("Quarter")

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

  a  <- quarter(testdat$a)
  b  <- quarter(testdat$b)
  c  <- quarter(testdat$c)
  c2 <- quarter(testdat$c)
  d  <- quarter(testdat$d)
  e  <- quarter(testdat$e)
  f  <- quarter(testdat$f)

  expect_true(a < b)
  expect_true(b < c)
  expect_false(c < c2)
  expect_true(c <= c2)
  expect_true(c == c2)
  expect_true(c >= c2)
  expect_true(f > e)
  expect_true(e > d)
  expect_true(d > c2)

  x <- quarter(c(a, b, c))
})


test_that("Incrementing / Decrementing quarters works.", {
  x  <- with(testdat, quarter(c(a, b ,c ,d)))

  expect_identical(increment(x, 3), quarter(c("2014-Q3", "2014-Q4", "2015-Q1", "2015-Q2")))
})
