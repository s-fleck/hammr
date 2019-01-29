context("df_round")


test_that("df_round works as expected", {
  #* @testing df_round
  #* @testing df_signif

  tdat <- data.frame(
    x = c(0.1234567, 0.3, 23456.01),
    y = c(0.1234567, 0.2, 12345.0)
  )

  expect_identical(
    df_round(tdat, 1)$x,
    c(0.1, 0.3, 23456.0)
  )

  expect_identical(
    df_round(tdat, 1)$y,
    c(0.1, 0.2, 12345.0)
  )

  expect_identical(
    df_signif(tdat, 3)$x,
    c(0.123, 0.3, 23500)
  )

  expect_identical(
    df_signif(tdat, 3)$y,
    c(0.123, 0.2, 12300)
  )
})



test_that("df_round works with duplicated column names", {
  #* @testing df_round
  #* @testing df_signif

  tdat <- data.table::data.table(
    x = c(0.1234567, 0.3, 23456.01),
    x = c(0.1234567, 0.2, 12345.0)
  )

  expect_identical(
    df_round(tdat, 1)[[1]],
    c(0.1, 0.3, 23456.0)
  )

  expect_identical(
    df_round(tdat, 1)[[2]],
    c(0.1, 0.2, 12345.0)
  )
})


test_that("df_round ignore works", {
  #* @testing df_round
  #* @testing df_signif

  tdat <- data.table::data.table(
    x = c(0.1234567, 0.3, 23456.01),
    x = c(0.1234567, 0.2, 12345.0),
    y = c(1.2, 1.2, 1.3)
  )

  iy <- df_round(tdat, ignore = "y")
  expect_false(identical(iy[[1]], tdat[[1]]))
  expect_false(identical(iy[[2]], tdat[[2]]))
  expect_identical(iy[[3]], tdat[[3]])

  ix <- df_round(tdat, ignore = "x")
  expect_identical(ix[[1]], tdat[[1]])
  expect_identical(ix[[2]], tdat[[2]])
  expect_false(identical(ix[[3]], tdat[[3]]))
})
