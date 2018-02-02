context("df_margins")


test_that("df_margins works as expected", {

  mc1 <-  1:5
  class(mc1) <- c('classy', class(mc1))

  mc2 <-  1:5
  class(mc2) <- c('not_classy', class(mc2))

  tdat <- data.frame(
    num = as.numeric(c(1, 2, NA_real_, 3, 5)),
    int = 1L:5L,
    chr = LETTERS[1:5],
    fct = factor(LETTERS[1:5]),
    time = Sys.time(),
    date = Sys.Date(),
    `funk column name %%%` = 11:15,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  eres <- list(
    num  = 11,
    int  = 15L,
    chr  = "",
    fct  = as.factor(""),
    time = NA,
    date = NA,
    `funk column name %%%` = 99
  )


  tres <- get_margin_row(tdat, sum_name = list(`funk column name %%%` = 99), na.rm = TRUE)
  expect_identical(eres, tres)


  expect_warning(r1 <- df_add_margin_row(tdat))
  expect_warning(r2 <- df_add_margin_row(tibble::as.tibble(tdat)))
  expect_warning(r3 <- df_add_margin_row(
    data.table::as.data.table(tdat),
    chr = "F"
  ))


  expect_identical(class(r1), "data.frame")
  expect_identical(class(r2), c("tbl_df", "tbl", "data.frame"))
  expect_identical(class(r3), c("data.table", "data.frame"))

})
