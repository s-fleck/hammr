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

