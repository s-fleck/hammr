context("char_to_cols")


test_that("char_to_cols works as expected", {

  tres <- char_to_colspec(c("character", "numeric", "double"))

  expect_true(inherits(tres, "col_spec"))



})
