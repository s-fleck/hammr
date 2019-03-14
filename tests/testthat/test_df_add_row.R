context("df_add_row")


test_that("df_add_row works as expected", {

  x <- iris

  tres1 <- df_add_row(x, Species = "blubb")

  expect_s3_class(tres1, "data.frame")
  expect_true(!data.table::is.data.table(tres1))

  tres2 <- df_add_row(data.table::as.data.table(x), Species = "blubb")
  expect_true(data.table::is.data.table(tres2))
})
