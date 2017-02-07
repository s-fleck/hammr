context("df_na0")


test_that("df_na0 works as expected", {
  tdat <- data.table::data.table(
    x = c(1, NA, 0),
    y = c(1, NaN, 2)
  )

  r1 <- df_na0(tdat)
  r2 <- df_na0(as.data.frame(tdat))

  expect_identical(
    r1$x[[2]], 0
  )

  expect_identical(
    r1$y[[2]], 0
  )


  # Results for .data.frame and .data.table method must match
  expect_identical(
    as.data.frame(r1), r2
  )

#   tdat2 <- data.table::data.table(
#     x = as.factor(c(1, NA, 0)),
#     y = as.factor(c(1, NaN, 2))
#   )
})
