context("Sort edge pairs")


# setwd(system.file('tests', 'testthat', package = 'hammr'))


# Setup test data ----

  # Data will be scrambled/sorted nit times. Higher numbers will slow down
  # testing process and are likely not necessary if you do not modify the
  # sorting alogirthm

  nit <- 2L


  # Must be sorted (will be scrambled for testing and compared with original data)
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

  tdat_rw <- data.table::data.table(
    p    = c(NA, "3023", "2181"),
    c    = c("3023", "2181", "3023"),
    n    = c("2181", "3023", NA),
    col1 = c('a', 'b', 'c')
  )

  tchain24 <- readRDS('test_data/sort_edge_pairs/tchain24.rds') %>%
    dplyr::rename(p = befprev,
                  c = bef,
                  n = befnext)
  tchain24 <- tchain24[c(3, 1, 2)]


  test_that("sort edge pairs", {

  testfun <- function(testdata, iterations = nit){
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



test_that("sort edge pair data.frame works", {

  testfun <- function(testdata, iterations = nit){
    require(foreach)
    require(magrittr)
    res <- foreach(i = 1:iterations, .combine = c) %do% {

      testdata_scrambled <- testdata %>%
        dplyr::sample_frac()

      sorted <- sort_edge_pairs(testdata)

      identical(sorted, testdata)
    }

    all(res)
  }

  # Best case scenarion
    expect_true(testfun(best_sorted))

  # Best case scenario (but first elemen is NA)
    expect_true(testfun(na_sorted))

  # Duplicated nodes
    expect_true(testfun(tdat_rw))
    expect_true(testfun(tchain24))

  # Missing links
    expect_error(testfun(unconnected_sorted))
    expect_error(testfun(unconnected_2, 10))

  # Classes are assigned correctly
    tdat1 <- na_sorted %>%
      dplyr::sample_frac()

    tdat2 <- na_sorted %>%
      dplyr::sample_frac() %>%
      as.data.frame()

    res1 <- sort_edge_pairs(tdat1)
    res2 <- sort_edge_pairs(tdat2)

    expect_identical(as.data.frame(res1), res2)

    expect_identical(class(res1), c('data.table', 'data.frame'))
    expect_identical(class(res2), c('data.frame'))
})



test_that("partial sort edge pair df works", {

  testfun <- function(testdata, iterations = nit){
    require(foreach)
    require(magrittr)
    res <- foreach(i = 1:iterations, .combine = c) %do% {

      testdata_scrambled <- testdata %>%
        dplyr::sample_frac()

      sorted <- sort_edge_pairs(testdata, allow_partial = TRUE)
      sorted[, .id := NULL]
      identical(sorted, testdata)
    }

    all(res)
  }


  # Best case scenarion
  expect_true(testfun(best_sorted))

  # Best case scenario (but first elemen is NA)
  expect_true(testfun(na_sorted))

  # Duplicated nodes
  expect_true(testfun(tdat_rw))
  expect_true(testfun(tchain24))

  # Missing links
  expect_silent(testfun(unconnected_sorted))
  expect_silent(testfun(unconnected_2, 10))
})


test_that('node sort uitility functions work', {
  expect_true(is_sorted_edge_pairs(best_sorted$p, best_sorted$c, best_sorted$n))
  expect_true(is_sorted_edge_pairs(na_sorted$p, na_sorted$c, na_sorted$n))
  expect_false(is_sorted_edge_pairs(unconnected_sorted$p, unconnected_sorted$c, unconnected_sorted$n))
  expect_false(is_sorted_edge_pairs(unconnected_2$p, unconnected_2$c, unconnected_2$n))
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
