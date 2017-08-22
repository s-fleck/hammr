context("ape")


test_that("ape works as expected", {

  x <- data.frame(
    t = c(1, 2, 3),
    p = c(1.1, 1.9, 3.1),
    w = c(0, 2, 0)
  )


expect_equal(ape(x$t, x$p), c(10, 5, 10/3))
expect_equal(pe(x$t, x$p), c(10, -5, 10/3))
expect_equal(weighted_mape(x$t, x$p), mean(ape(x$t, x$p)))
expect_equal(weighted_mpe(x$t, x$p),  mean(pe(x$t, x$p)))
expect_equal(weighted_mape(x$t, x$p, x$w), 5)
expect_equal(weighted_mpe(x$t, x$p, x$w), -5)




})
