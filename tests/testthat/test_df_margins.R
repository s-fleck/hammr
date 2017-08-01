context("df_margins")


test_that("df_margins works as expected", {

  mc1 <-  1:5
  class(mc1) <- c('classy', class(mc1))

  mc2 <-  1:5
  class(mc2) <- c('not_classy', class(mc2))

  tdat <- data.frame(
    num = as.numeric(1:5),
    int = 1L:5L,
    chr = LETTERS[1:5],
    fct = factor(LETTERS[1:5]),
    time = Sys.time(),
    date = Sys.Date(),
    cls1 = mc1,
    cls2 = mc2,
    stringsAsFactors = FALSE
  )

  eres <- data.frame(
    num  = 15,
    int  = 15L,
    chr  = "",
    fct  = "",
    time = NA,
    date = NA,
    cls1  = 99L,
    cls2 = NA
  )


  tres <- get_margin_row(tdat, classy = 99L)
  expect_identical(eres, tres)


  expect_silent(df_add_margin_row(tdat))



})
