context("Utils")

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
  stringsAsFactors = FALSE
)


test_that("mass typecasting data.frame columns works.", {
  res <- typecast_all(testdat, 'factor', 'character')

  expect_identical(res$a, c("6", "5", "3", "4", "5"))
  expect_identical(res$b, c('one', 'two', 'three', 'four', ' apple '))
  expect_identical(res$c, testdat$c)
  expect_identical(res$d, testdat$d)
})

test_that("typecasting by name works.", {

  res <- typecast_cols(testdat, list(
    a = 'numeric',
    b = 'character',
    c = 'factor',
    d = 'factor',
    e = 'integer',
    f = 'factor',
    g = 'logical',
    h = 'integer',
    i = 'character',
    j = c("POSIXct", "POSIXt")
  ))

  expect_identical(res$a, c(6, 5, 3, 4, 5))
  expect_identical(res$b, c("one", "two", "three", "four", " apple "))
  expect_identical(res$c, factor(testdat$c))
  expect_identical(res$d, factor(testdat$d))
  expect_identical(res$e, c(1L, 1L, 1L, 1L, 1L))
  expect_identical(res$f, factor(testdat$f))
  expect_identical(res$g, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(res$h, c(3L, 4L, 5L, 5L, 5L))
  expect_identical(res$i, c("2015-01-01", "2015-01-05", "2015-05-04", "2015-12-01", "2015-04-13"))



})


test_that("removing whitespaces from character columns of data frame works.", {
  res <- remove_whitespace(testdat)
  res2 <- remove_whitespace(testdat, process_factors = TRUE)

  expect_identical(res$a, testdat$a)
  expect_identical(res$b, testdat$b)
  expect_identical(res$c, c('one', 'two', 'three', 'four', 'apple'))
  expect_identical(res$d, testdat$d)

  expect_identical(levels(res2$b), c("apple", "four", "one", "three", "two"))

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


test_that("Dropping columns by name works.", {
  res <- list()

  res$a <- drop_if_exists(testdat, 'j')
  res$b <- drop_if_exists(testdat, c("a", "b", "c", 'dog', 'j'))

  expect_equal(names(res$a), c("a", "b", "c", "d", "e", "f", "g", "h", "i"))
  expect_equal(names(res$b), c("d", "e", "f", "g", "h", "i"))

})

# test_that('Redfined + operator does not break stuff', {
#
#   expect_identical('would' + 'could', 'wouldcould')
#   expect_identical((c('a', 'b') + c('b', 'c')), c("ab", "bc"))
#   expect_identical(1+1, 2)
#   expect_error(1 + '2')
#
#   if(require(ggplot2) == TRUE){
#   p <- ggplot(testdat,
#          aes(x = a,
#              y = b)) + geom_bar(stat = 'identity')
#   }
# })

