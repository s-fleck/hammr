context("Utils")

testdat <- data.frame(
  a = factor(c(6,5,3,4,5)),
  b = factor(c('one', 'two', 'three', 'four', ' apple ')),
  c = c(' one ', ' two ', ' three ', ' four ', ' apple '),
  d = c(TRUE, TRUE, TRUE, FALSE, FALSE),
  e = c(1, 1, 1, 1, 1),
  f = c('moon', 'moon', 'moon', 'moon', 'moon'),
  stringsAsFactors = FALSE
)


test_that("mass typecasting data.frame columns works.", {
  res <- typecast_all(testdat, 'factor', 'character')

  expect_identical(res$a, c("6", "5", "3", "4", "5"))
  expect_identical(res$b, c('one', 'two', 'three', 'four', ' apple '))
  expect_identical(res$c, testdat$c)
  expect_identical(res$d, testdat$d)
})


test_that("removing whitespaces from character columns of data frame works.", {
  res <- remove_whitespace(testdat)

  expect_identical(res$a, testdat$a)
  expect_identical(res$b, testdat$b)
  expect_identical(res$c, c('one', 'two', 'three', 'four', 'apple'))
  expect_identical(res$d, testdat$d)
})



test_that("all_identical works.", {
  res <- remove_whitespace(testdat)

  expect_false(all_identical(testdat$a))
  expect_false(all_identical(testdat$b))
  expect_false(all_identical(testdat$c))
  expect_false(all_identical(testdat$d))
  expect_true(all_identical(testdat$e))
  expect_true(all_identical(testdat$f))
})



test_that("string chopping works.", {
  x = 'abc defg hijklmnop  999 end'

  breaks <- c(1, 3,8, 18, 99999999)

  res <- str_chop(x, breaks)

  expect_identical(res[1], 'abc')
  expect_identical(res[2], ' defg')
  expect_identical(res[3], ' hijklmnop')
  expect_identical(res[4], '  999 end')
})


test_that("string prioritizing works.", {

  res <- list()

  res$a  <- prioritize_char(testdat$a, 'blubb')
  res$a2 <- prioritise_char(testdat$a, 'blubb')
  res$b  <- prioritize_char(testdat$b, high = c('four', 'three'), low = c('one', 'two'))

  expect_identical(res$a, as.character(testdat$a))
  expect_identical(res$a, res$a2)
  expect_identical(res$b, c("four", "three", " apple ", "one", "two"))
})


test_that("factor prioritizing works.", {
  res <- list()

  res$b   <- prioritize_factor(testdat$b, high = c('four', 'three'), low = c('one', 'two'))
  res$b2  <- prioritize_factor(testdat$b, high = c('peach', 'car'), low = c('house', ' apple '))

  expect_identical(res$b, structure(c(4L, 5L, 2L, 1L, 3L),
                                    .Label = c("four", "three", " apple ", "one", "two"),
                                    class = "factor"))

  expect_identical(res$b2, structure(c(2L, 4L, 3L, 1L, 5L),
                                    .Label = c("four", "one", "three", "two", " apple "),
                                    class = "factor"))
})


test_that('Redfined + operator does not break stuff'){

expect_identical('would' + 'could', 'wouldcould')
expect_identical((c('a', 'b') + c('b', 'c')), c("ab", "bc"))
expect_identical(1+1, 2)
expect_error(1 + '2')

if(require(ggplot2) == TRUE){
p <- ggplot(testdat,
       aes(x = a,
           y = b)) + geom_bar(stat = 'identity')
}
}
