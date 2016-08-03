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

  #"2013-Q4" "2014-Q1" "2014-Q2" "2014-Q3"

  expect_identical(increment(x, 3),  quarter(c("2014-Q3", "2014-Q4", "2015-Q1", "2015-Q2")))
  expect_identical(increment(x, 7),  quarter(c("2015-Q3", "2015-Q4", "2016-Q1", "2016-Q2")))
  expect_identical(increment(x, -4), quarter(c("2012-Q4", "2013-Q1", "2013-Q2", "2013-Q3")))
  expect_identical(increment(x, -7), quarter(c("2012-Q1", "2012-Q2", "2012-Q3", "2012-Q4")))

})




