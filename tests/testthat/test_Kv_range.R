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
  test_that("within_kv_range: checking whether a value is present for a specific
            key and a specific range", {

    res <- testdat

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


    expect_true(within_kv_range(testdat$key[1], testdat$val[1], test_range2, 'key'))
    expect_false(within_kv_range(testdat$key[4], testdat$val[4], test_range2, 'key'))

    expect_error(within_kv_range(testdat$key[1], testdat$val, test_range2, 'key'))
    expect_error(within_kv_range(testdat$key[1], testdat$val, test_range2, 'keybu'))
  })

