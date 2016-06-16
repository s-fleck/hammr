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
  k = c('1', '1.5', '0.000000001', '100000000000', '99.1'),
  l = factor(c('1', '1.5', '0.000000001', '100000000000', '99.1')),
  m = factor(c('a', NA, NA, NA, NA)),
  n = factor(c(NA, NA, NA , NA, NA)),
  n2 = c('apple', 'applepie', 'moon', 'nomoon', 'moo'),
  stringsAsFactors = FALSE
)


test_that("mass typecasting data.frame columns works.", {
  res <- typecast_all(testdat, 'factor', 'character')

  expect_identical(res$a, c("6", "5", "3", "4", "5"))
  expect_identical(res$b, c('one', 'two', 'three', 'four', ' apple '))
  expect_identical(res$c, testdat$c)
  expect_identical(res$d, testdat$d)


  tdat <- data.frame(
    a = c(NA, NA, "-1", "9999", "one", "6", NA, NA, "3", "1", "2", "1", "0", NA, NA, NA, NA, NA, NA, NA)
  )

  conv = list(a = 'integer')

  expect_warning(res <- typecast_cols(tdat, conv = conv))
  expect_identical(res, data.frame(a = as.integer(c(NA, NA, -1L, 9999L, NA, 6L, NA, NA, 3L, 1L, 2L, 1L, 0L, NA, NA, NA, NA, NA, NA, NA)) )
  )
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
  res  <- remove_whitespace(testdat)
  res2 <- remove_whitespace(testdat, process_factors = TRUE)

  expect_identical(res$a, testdat$a)
  expect_identical(res$b, testdat$b)
  expect_identical(res$c, c('one', 'two', 'three', 'four', 'apple'))
  expect_identical(res$d, testdat$d)

  expect_identical(levels(res2$b), c("apple", "four", "one", "three", "two"))
  expect_identical(res2$m, factor(c("a", NA, NA, NA, NA)))

})


test_that('Custom type conversion functions work', {
  y <- factor('99')

  expect_identical(as.integer2(y), 99L)
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

  res$c <- drop_if_exists(testdat, c("n"))

  expect_equal(names(res$a)[1:9], c("a", "b", "c", "d", "e", "f", "g", "h", "i"))
  expect_equal(names(res$b)[1:6], c("d", "e", "f", "g", "h", "i"))
  expect_equal(names(res$c)[1:14], c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n2"))

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


test_that("Looks like integer works.", {
  a <- looks_like_integer(testdat$k)
  b <- looks_like_integer(testdat$l)
  c <- looks_like_integer(as.numeric(testdat$k))

  expect_identical(a, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(a, b)
  expect_identical(a, c)
})


test_that("Calculation of urs / ukz checksum works", {
  x <- c("Z008W3509", "Z004L4056", "Z000A9205", "Z002E173Y", "Z005I796U", "Z005I796U", "Z001U954W", "Z000A212W", "Z001Z960U", "Z006C660J", "Z007U042V",
         "Z005M317G", "Z090V175H", "Z008M7137", "Z009L496E", "Z011V721N", "Z011A739M", "Z020S621A", "Z018M5022", "Z010J6414", "Z005M316I", "Z021I865W",
         "Z002B329Z", "Z002E005R", "Z012Y470N", "Z093B129A", "Z093U465S", "Z093Z9216", "Z093Z9224", "Z091Q4213", "Z020A2423", "Z019E997K")

  test_cases <- substring(x, 1, 8)
  should     <- substring(x, 9, 9)

  is <- calc_urs_check_digit(test_cases)

  expect_identical(should, is)

})


