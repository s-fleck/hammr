context("df_na0")


test_that("df_na_replace works as expected", {
  #* @testing df_na_replace
  #* @testing df_na_replace.data.frame
  #* @testing df_na_replace.data.table
  #* @testing df_na0


  tdat <- data.table::data.table(
    character = c('1', 'NA', NA),
    numeric = c(1, NA, 0),
    integer = c(1L, NaN, 2L),
    factor  = factor(c('a', 'NA', NA))
  )

  expect_warning(
    r1 <- df_na0(tdat),
    'is outside the levels range'
  )

  expect_warning(
    r2 <- df_na0(as.data.frame(tdat)),
    'invalid factor level'
  )

  # Results for .data.frame and .data.table method must match
  expect_identical(
    as.data.frame(r1), r2
  )

  eres12 <- data.frame(
    character = c("1", "NA", "0"),
    numeric = c(1, 0, 0),
    integer = c(1, 0, 2),
    factor = factor(c("a", "NA", NA)),
    stringsAsFactors = FALSE
  )

  expect_identical(r1, data.table::as.data.table(eres12))
  expect_identical(r2, eres12)


  expect_warning(df_replace_na(tdat, ''))
  expect_silent(df_replace_na(tdat, '', as_char = TRUE))
  expect_silent(r3 <- df_na_blank(tdat))
  expect_warning(df_replace_na(as.data.frame(tdat), ''))
  expect_silent(r4 <- df_replace_na(as.data.frame(tdat), '', as_char = TRUE))

  eres34 <- data.frame(
    character = c("1", "NA", ""),
    numeric = c("1", "", "0"),
    integer = c("1", "", "2"),
    factor = c("a", "NA", ""),
    stringsAsFactors = FALSE
  )

  expect_identical(r3, data.table::as.data.table(eres34))
  expect_identical(r4, eres34)


  # replace_na_string behaves as expected
    eres4 <- data.table::data.table(
      character = c("1", "", ""),
      numeric = c("1", "", "0"),
      integer = c("1", "", "2"),
      factor = c("a", "", "")
    )

    expect_equal(
      df_replace_na(tdat, "", replace_na_string = TRUE, as_char = TRUE),
      eres4
    )

    expect_equal(
      df_replace_na(as.data.frame(tdat), "", replace_na_string = TRUE, as_char = TRUE),
      as.data.frame(eres4)
    )
})
