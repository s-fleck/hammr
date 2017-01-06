context('string chopping')

test_that("str_chop: string chopping works.", {
  x = 'abc defg hijklmnop  999 end'

  breaks <- c(1, 3,8, 18, 99999999)

  res <- str_chop(x, breaks)

  expect_identical(res[1], 'abc')
  expect_identical(res[2], ' defg')
  expect_identical(res[3], ' hijklmnop')
  expect_identical(res[4], '  999 end')
})
