context("Key-value Range")

# Setup test data ----
  test_range <- data.frame(
    from = as.Date(c("2008-01-01", "1008-01-01", "2999-09-30", "2013-10-01", "2013-01-01", "2013-01-01", '2014-01-01', NA, '2999-09-30')),
    to   = as.Date(c("2008-01-01", "1008-01-01", "2999-09-30", "2013-10-01", "2013-01-01", "2013-01-01", '2999-09-30', NA, '2999-09-30')) + 10,
    key  = c('apple', 'apple', 'pear', 'orange', 'dog', 'horse', 'Z000A212W', NA, NA),
    stringsAsFactors = FALSE
  ) %>% kv_range()


  test_range2 <- data.frame(
    from = as.Date(c("2008-01-01", "1008-01-01", "2999-09-30", "2013-10-01", "2013-01-01", "2013-01-01", '2014-01-01')),
    to   = as.Date(c("2008-01-01", "1008-01-01", "2999-09-30", "2013-10-01", "2013-01-01", "2013-01-01", '2999-09-30')) + 10,
    key  = c('apple', 'apple', 'pear', 'orange', 'dog', 'horse', 'Z000A212W'),
    stringsAsFactors = FALSE
  ) %>% kv_range()


  testdat <- data.frame(
    key  = c('apple', 'apple', 'pear', 'orange', 'dog', 'unicorn', 'Z000A212W', 'Z000A212W', 'dog', NA, NA),
    val = as.Date(c("1008-01-01", "2008-01-01", "2999-09-30", "2013-10-01", "2013-01-01", "2013-01-01", "2015-10-01", "3000-10-01", '2013-01-02', '2013-01-02', '2013-01-02')) + c(0, 0, 10, 11, -1, 0, 0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )


# Run the tests ----
  test_that("checking whether a value lies withing a valid_range works (test data)", {

    res <- testdat

    expect_warning(res$valid <- within_valid_range(keycol = testdat$key, value = testdat$val, test_range = test_range, check_key = 'key'))
    expect_warning(res$valid <- within_kv_range(testdat$key, testdat$val, test_range, 'key'))

    expect_true(res$valid[[1]])
    expect_true(res$valid[[2]])
    expect_true(res$valid[[3]])

    expect_false(res$valid[[4]])
    expect_true(res$valid[[5]])
    expect_false(res$valid[[6]])

    expect_true(res$valid[[7]])
    expect_true(res$valid[[8]])

    expect_true(res$valid[[9]])


    expect_warning(expect_true(within_valid_range(testdat$key[1], testdat$val[1], test_range2, check_key = 'key')))
    expect_true(within_kv_range(testdat$key[1], testdat$val[1], test_range2, 'key'))
    expect_warning(expect_false(within_valid_range(testdat$key[4], testdat$val[4], test_range2, check_key = 'key')))
    expect_false(within_kv_range(testdat$key[4], testdat$val[4], test_range2, 'key'))


    expect_warning(expect_error(within_valid_range(testdat$key[1], testdat$val, test_range2, check_key = 'key')))
    expect_error(within_kv_range(testdat$key[1], testdat$val, test_range2, 'key'))
    expect_warning(expect_error(within_valid_range(testdat$key, testdat$val, test_range2, check_key = 'keybu')))
    expect_error(within_kv_range(testdat$key[1], testdat$val, test_range2, 'keybu'))

  })


  # test_that("checking whether a value lies withing a valid_range works (real-world data)", {
  #   test_range <- readRDS('test_data/valid_range/test_range.rds')
  #   keycol     <- readRDS('test_data/valid_range/keycol.rds')
  #   value      <- readRDS('test_data/valid_range/value.rds')
  #   res1       <- readRDS('test_data/valid_range/res1.rds')
  #
  #
  #   expect_warning(tmp <- all(within_valid_range(keycol = keycol, value = value, test_range = test_range, check_key = 'pr')))
  #   expect_warning(tmp <- all(within_kv_range(keycol, value, test_range, 'pr')))
  #   expect_false(tmp)
  #
  #   expect_warning(tmp <- within_valid_range(keycol = keycol, value = value, test_range = test_range, check_key = 'rics'))
  #   expect_warning(tmp <- within_kv_range(keycol, value, test_range, 'rics'))
  #
  #   expect_identical(res1, tmp)
  # })
