context("Misc utils")




test_that("read_rda works", {
  x <- c(1, 2, 3, NA, 4, 5, NA)
  y <- c(1, 2, 3, NA, 7, 8, 9)

  td <- tempdir()
  f1 <- file.path(td, 'single.rda')
  f2 <- file.path(td, 'double.rda')

  save(x, file = f1)
  save(y, x, file = f2)

  expect_silent(expect_identical(
    read_rda(f1),
    x
  ))

  expect_warning(expect_identical(
    read_rda(f2),
    y
  ),
  'double.rda contains more than one object. Returning only the first: y'
  )

  file.remove(f1)
  file.remove(f2)
})
