stthcontext("df_transpose")


test_that("df_transpose works as expected", {

  tdat <- data.frame(
    x = LETTERS[1:5],
    y = 1:5
  )

  edat <- data.frame(
    A = 1L,
    B = 2L,
    C = 3L,
    D = 4L,
    E = 5L,
    stringsAsFactors = FALSE
  )

  expect_identical(
    df_transpose(tdat),
    edat
  )

  expect_identical(
    df_transpose(df_transpose(tdat)),
    tdat
  )



})
