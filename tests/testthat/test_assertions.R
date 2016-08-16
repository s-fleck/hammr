context("Assertions")

testdat <- quarter('2015-Q1')


test_that("mass typecasting data.frame columns works.", {
  expect_warning('blah' %is_class% 'character')
  expect_silent('blah' %assert_class% 'character')
})



