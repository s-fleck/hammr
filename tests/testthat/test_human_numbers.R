context("human_numbers")


test_that("human_numbers works as expected", {
  x <- c(-1e3, 0, -0, NA, 1e7, 1e9, 1e12, 1e15)

  expect_identical(
    human_num(x),
    c("-1k", "0", "0", NA, "10m", "1b", "1,000b", "1,000,000b")
  )

  expect_identical(
    human_numbers(x, pots = c('k' = 1e3, 'M' = 1e6, 'G' = 1e9, 'T' = 1e12)),
    c("-1k", "0", "0", NA, "10M", "1G", "1T", "1,000T")
  )

  expect_identical(
    human_numbers(x, symbol = '$'),
    c("-$1k", "$0", "$0", NA, "$10m", "$1b", "$1,000b", "$1,000,000b")
  )

  expect_identical(
    human_numbers(x, symbol = '$', big_mark = "'"),
    c("-$1k", "$0", "$0", NA, "$10m", "$1b", "$1'000b", "$1'000'000b")
  )


  # Deal with numbers between 0 and 1
    x2 <- c(0, 0.1, 0.9, 100000000, NA)

    expect_identical(
      human_num(x2),
      c("0", "0.1", "0.9", "100m", NA)
    )
})




test_that("human_time works as expected", {

  x <- difftime(as.Date("2019-12-01"), as.Date("2018-12-01"))

  expect_identical(human_time(x), "1y")
  expect_identical(human_time(x/2), "182.5d")
  expect_identical(human_time(x/365), "1d")
  expect_identical(human_time(x/400), "21.9h")
  expect_identical(human_time(x/(60*24*360)), "1m")
  expect_identical(human_time(x/(60*60*24*360)), "1s")

  expect_identical(human_time(-x), "-1y")


})

