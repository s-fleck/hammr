context("Misc utils")

test_that("unique_single works", {
  expect_identical(1L, unique_single(c(1L, 1L, 1L)))
  expect_error(unique_single(c(2L, 1L, 1L)))
})


test_that("unique_single works", {
  x <- c(1, 2, 3, NA, 4, 5, NA)
  y <- c(1, 2, 3, NA, 7, 8, 9)

  expect_identical(
    equal_or_both_na(x, y),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )

})
