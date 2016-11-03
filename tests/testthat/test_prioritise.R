context('prioritise')

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


test_that("string prioritizing works.", {

  res <- list()

  expect_warning(prioritize(as.character(testdat$a), 'blubb'))



  expect_warning(res$a  <- prioritize(as.character(testdat$a), 'blubb'))
  expect_warning(res$a2 <- prioritize(as.character(testdat$a), 'blubb'))
  res$b  <- prioritise(as.character(testdat$b), high = c('four', 'three'), low = c('one', 'two'))

  expect_identical(res$a, as.character(testdat$a))
  expect_identical(res$a, as.character(res$a2))
  expect_identical(res$b, c("four", "three", " apple ", "one", "two"))
})


test_that("factor prioritizing works.", {
  res <- list()

  res$b   <- prioritize(testdat$b, high = c('four', 'three'), low = c('one', 'two'))
  expect_warning(
    res$b2  <- prioritise(testdat$b, high = c('peach', 'car'), low = c('house', ' apple '))
  )

  expect_identical(res$b, structure(c(4L, 5L, 2L, 1L, 3L),
                                    .Label = c("four", "three", " apple ", "one", "two"),
                                    class = "factor"))

  expect_identical(res$b2, structure(c(2L, 4L, 3L, 1L, 5L),
                                     .Label = c("four", "one", "three", "two", " apple "),
                                     class = "factor"))
})

