context("Assertions")

testdat <- quarter('2015-Q1')


test_that("mass typecasting data.frame columns works.", {
  expect_silent('blah' %is_class% 'character')
})



