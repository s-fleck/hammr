context("Misc utils")


test_that("unique_single works", {
  expect_identical(1L, unique_single(c(1L, 1L, 1L)))
  expect_error(unique_single(c(2L, 1L, 1L)))
})

