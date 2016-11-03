context('df_remove_whitespace')

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

test_that("removing whitespaces from character columns of data frame works.", {
  res  <- remove_whitespace(testdat)
  res2 <- remove_whitespace(testdat, process_factors = TRUE)

  expect_identical(res$a, testdat$a)
  expect_identical(res$b, testdat$b)
  expect_identical(res$c, c('one', 'two', 'three', 'four', 'apple'))
  expect_identical(res$d, testdat$d)

  expect_identical(levels(res2$b), c("apple", "four", "one", "three", "two"))
  expect_identical(res2$m, factor(c("a", NA, NA, NA, NA)))

})
