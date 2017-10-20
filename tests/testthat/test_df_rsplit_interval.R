context("df_rsplit_interval")


test_that("df_rsplit_interval works as expected", {

  tres <- df_rsplit_interval(iris)

  expect_identical(
    iris,
    as.data.frame(data.table::rbindlist(tres))
  )


})
