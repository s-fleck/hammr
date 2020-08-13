context("df_drop_empty")


test_that("df_drop_empty works as expected", {

  dat <- data.frame(
    x = c(1, NA, NA),
    y = c("A", NA_character_, NA_character_),
    z = c("sd", "", NA_character_),
    stringsAsFactors = FALSE
  )

  expect_identical(nrow(df_drop_empty_rows(dat)), 2L)
  expect_identical(nrow(df_drop_empty_rows(dat, drop_blanks = TRUE)), 1L)

  dat <- data.frame(
    x = c(1, NA, NA),
    y = factor(c("A", NA_character_, NA_character_)),
    z = factor(c("sd", "", NA))
  )

  expect_identical(nrow(df_drop_empty_rows(dat)), 2L)
  expect_identical(nrow(df_drop_empty_rows(dat, drop_blanks = TRUE)), 1L)
})
