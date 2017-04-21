context("date_yq")


test_that("date_yq works as expected", {
  expect_silent(tr1 <- date_yq(2015, 2))

  expect_identical(as.Date(tr1), as.Date('2015-04-01'))
  expect_identical(as.Date(tr1), as.Date('2015-04-01'))

  tr2 <- as_date_yq(20152)
  expect_identical(tr1, tr2)

  tr3 <- as_date_yq(as.Date('2015-05-02'))
  expect_identical(tr1, tr3)


  expect_identical(format(tr1), "2015-Q2")
  expect_identical(format(tr1, 'short'), "2015.2")
  expect_identical(format(tr1, 'shorter'), "15.2")

})
