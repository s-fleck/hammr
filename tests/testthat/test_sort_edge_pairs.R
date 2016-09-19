context("Sort edge pairs")


# Setup test data ----
  small_sorted <- data.table::data.table(
    p = c(NA_character_, '2181'),
    c = c('2181', '2243'),
    n = c("2243", '2155')
  )


  best_sorted <- data.table::data.table(
    p = letters[1:20],
    c = letters[2:21],
    n = letters[3:22],
    col1 = 1:20,
    col2 = LETTERS[1:20]
  )

  na_sorted <- data.table::data.table(
    p = c(NA_character_, letters[2:20]),
    c = letters[2:21],
    n = c(letters[3:21], NA_character_),
    col1 = 1:20,
    col2 = LETTERS[1:20]
  )

  unconnected_sorted  <- best_sorted[-c(3,4,5)]

  unconnected_2 <- data.table::data.table(
    p = sample(best_sorted$p),
    c = sample(best_sorted$c),
    n = sample(best_sorted$n),
    col1 = 1:20,
    col2 = LETTERS[1:20]
  )



test_that("sort edge pairs", {

  testfun <- function(testdata, iterations = 10L){
    require(foreach)
    res <- foreach(i = 1:iterations, .combine = c) %do% {

      testdata_scrambled <- testdata %>%
        dplyr::sample_frac()

      sorted <- sort_edge_pairs(
        testdata_scrambled$p,
        testdata_scrambled$c,
        testdata_scrambled$n
      )

      identical(sorted, testdata[,.(p, c, n)])
    }

    all(res)
  }


  # Best case scenarion
  expect_true(with(best_sorted, is_sortable_edge_pairs(p, c, n)))
  expect_true(testfun(best_sorted))


  # Small data set (2 rows)
  expect_true(with(small_sorted, is_sortable_edge_pairs(p, c, n)))
  expect_true(testfun(small_sorted))


  # Best case scenario (but first elemen is NA)
  expect_true(with(na_sorted, is_sortable_edge_pairs(p, c, n)))
  expect_true(testfun(na_sorted))


  # Missing links
  expect_false(with(unconnected_sorted, is_sortable_edge_pairs(p, c, n)))
  expect_error(testfun(unconnected_sorted))

  expect_false(with(unconnected_2, is_sortable_edge_pairs(p, c, n)))
  expect_error(testfun(unconnected_2, 10))
})




test_that("sort edge pair df works", {

  testfun <- function(testdata, iterations = 10L){
    require(foreach)
    res <- foreach(i = 1:iterations, .combine = c) %do% {

      testdata_scrambled <- testdata %>%
        dplyr::sample_frac()

      sorted <- sort_edge_pair_df(testdata)

      identical(sorted, testdata)
    }

    all(res)
  }


  # Best case scenarion
  expect_true(testfun(best_sorted))


  # Best case scenario (but first elemen is NA)
  expect_true(testfun(na_sorted))


  # Missing links
  expect_error(testfun(unconnected_sorted))

  expect_error(testfun(unconnected_2, 10))


})


test_that('node sort uitility functions work', {
  expect_true(is_node_sorted(best_sorted$p, best_sorted$c, best_sorted$n))
  expect_true(is_node_sorted(na_sorted$p, na_sorted$c, na_sorted$n))
  expect_false(is_node_sorted(unconnected_sorted$p, unconnected_sorted$c, unconnected_sorted$n))
  expect_false(is_node_sorted(unconnected_2$p, unconnected_2$c, unconnected_2$n))
})


test_that('getting next and previous elements works', {
  # Setup test data ----

  tdat <- list()

  tdat[[1]] <- data.table::data.table(
    p = letters[1:4],
    c = letters[2:5],
    n = letters[3:6]
  )

  tdat[[2]] <- data.table::data.table(
    p = letters[5:8],
    c = letters[6:9],
    n = letters[7:10]
  )

  tdat[[3]] <- data.table::data.table(
    p = letters[9:12],
    c = letters[10:13],
    n = letters[11:14]
  )




  # Run tests ----

  expect_identical(get_prv(tdat, tdat[[2]]), 1L)
  expect_identical(get_nxt(tdat, tdat[[2]]), 3L)
  expect_identical(get_nxt(tdat, tdat[[3]]), integer())
  expect_identical(get_prv(tdat, tdat[[1]]), integer())
})
