context("Misc utils")

test_that("unique_single works", {
  expect_identical(1L, unique_single(c(1L, 1L, 1L)))
  expect_error(unique_single(c(2L, 1L, 1L)))
})




test_that("unique_single works", {
  x <- c(1, 2, 3, NA, 4, 5, NA)
  y <- c(1, 2, 3, NA, 7, 8, 9)

  expect_identical(
    equal_or_na(x, y),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})





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





test_that("unique_single works", {
  x <- c(1, 2, 3, NA, 4, 5, NA)
  y <- c(1, 2, 3, NA, 7, 8, 9)

  expect_identical(
    equal_or_na(x, y),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})





test_that("all with warning works", {
  x <- c(TRUE, TRUE, FALSE, FALSE, NA, NA)
  y <- setNames(x, letters[seq_along(x)])
  z <- c(NA, TRUE, TRUE)

  expect_warning(
    expect_false(all_with_warning(x)),
    '3, 4, 5, 6$'
  )

  expect_warning(
    expect_false(all_with_warning(x, na_value = NA)),
    '3, 4, 5, 6$'
  )

  expect_warning(
    expect_false(all_with_warning(x, na_value = TRUE)),
    '3, 4$'
  )

  expect_warning(
    expect_false(tres <- all_with_warning(y)),
    'c, d, e, f$'
  )

  expect_identical(attr(tres, 'failed'), 3L:6L)


  expect_warning(
    expect_false(all_with_warning(y, na_value = NA)),
    'c, d, e, f$'
  )

  expect_warning(
    expect_false(all_with_warning(y, na_value = TRUE)),
    'c, d$'
  )


  expect_warning(
    expect_false(all_with_warning(z)),
    '1$'
  )

  expect_warning(
    expect_true(is.na(all_with_warning(z, na_value = NA))),
    '1$'
  )

  expect_true(all_with_warning(z, na_value = TRUE))
})

