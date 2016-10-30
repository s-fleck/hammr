context('df_drop_if_exists')

testdat <- data.frame(
  a = factor(c(6,5,3,4,5)),
  b = factor(c('one', 'two', 'three', 'four', ' apple ')),
  c = c(' one ', ' two ', ' three ', ' four ', ' apple '),
  d = c(TRUE, TRUE, TRUE, FALSE, FALSE),
  e = c(1, 1, 1, 1, 1),
  f = c('moon', 'moon', 'moon', 'moon', 'moon'),
  g = c('TRUE', 'TRUE', 'TRUE', 'FALSE', 'FALSE'),
  h = c('3', '4', '5', '5', '5'),
  i = as.POSIXct(c('2015-01-01', '2015-01-05', '2015-05-04', '2015-12-01', '2015-04-13')),
  j = c('2015-01-01', '2015-01-05', '2015-05-04', '2015-12-01', '2015-04-13'),
  k = c('1', '1.5', '0.000000001', '100000000000', '99.1'),
  l = factor(c('1', '1.5', '0.000000001', '100000000000', '99.1')),
  m = factor(c('a', NA, NA, NA, NA)),
  n = factor(c(NA, NA, NA , NA, NA)),
  n2 = c('apple', 'applepie', 'moon', 'nomoon', 'moo'),
  stringsAsFactors = FALSE
)


test_that("Dropping columns by name works.", {
  res <- list()

  res$a <- drop_if_exists(testdat, 'j')
  res$b <- drop_if_exists(testdat, c("a", "b", "c", 'dog', 'j'))

  res$c <- drop_if_exists(testdat, c("n"))

  expect_equal(names(res$a)[1:9], c("a", "b", "c", "d", "e", "f", "g", "h", "i"))
  expect_equal(names(res$b)[1:6], c("d", "e", "f", "g", "h", "i"))
  expect_equal(names(res$c)[1:14], c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n2"))

})
