context("most_frequent")


test_that("most_frequent works as expected", {
  tdat <- c('a', 'a', NA, NA, NA, 'b', 'c', 'c', 'c')

  expect_identical(most_frequent(tdat), NA_character_)
  expect_identical(most_frequent(tdat, na.rm = TRUE), 'c')
  expect_identical(most_frequent(tdat, n = 2), c(NA_character_, 'c'))
})
