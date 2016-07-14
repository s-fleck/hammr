context("Assertions")



test_that("mass typecasting data.frame columns works.", {
  expect_silent('blah' %is_class% 'character')
})



