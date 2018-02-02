context("vec_enum")


test_that("vec_enum works as expected", {
  x <- c(  1,  2,    2, 2)
  y <- c("b", NA, "NA", NA)
  z1 <- list(cars, cars, cars, cars)
  z2 <- list(cars, cars, cars, iris)

  expect_identical(
    vec_enum(x, y, z1), c(1L, 2L, 3L, 2L)
  )

  expect_identical(
    vec_enum(x, y, z2), c(1L, 2L, 3L, 4L)
  )
})

