context("df_complement")


test_that("df_complement works as expected", {

  df1 <- data.frame(
    g1 = letters[c(1, 2, 2, 2, 2, 3, 4, 5, 6, 1)],
    g2 = LETTERS[c(1, 2, 3, 4, 5, 5, 5, 2, 1, 1)],
    da = 1:10
  )

  df2 <- data.frame(
    g1 = letters[c(1, 2, 3, 6, 7)],
    g2 = LETTERS[c(1, 2, 2, 2, 7)],
    da = 6:10,
    du = 100:104
  )



  res <- df_complement(df1, list(g1 = df2$g1))

  r2 <- df_complement(
    df2,
    list(
      'g1' = df1$g1,
      'g2' = df1$g2)
  )

  tres <- data.table(
    g1 = structure(
      c(1L, 2L, 3L, 4L, 5L, 2L, 2L, 2L, 3L, 6L, 7L, 4L),
      .Label = c("a", "b", "c", "f", "g", "d", "e"),
      class = "factor"),
    g2 = structure(
      c(1L, 2L, 2L, 2L, 3L, 4L, 5L, 6L, 6L, 6L, 2L, 1L),
      .Label = c("A", "B", "G", "C", "D", "E"),
      class = "factor"),
    da  =c(6L, 7L, 8L, 9L, 10L, NA, NA, NA, NA, NA, NA, NA),
    du = c(100L, 101L, 102L, 103L, 104L, NA, NA, NA, NA, NA, NA, NA)
  )

  expect_identical(
    as.data.frame(r2), as.data.frame(tres)
  )

  grps <- union(paste(df1$g1, df1$g2), paste(df2$g1, df2$g2))

  expect_true(
    all(paste(r2$g1, r2$g2) %in% grps)
  )

  expect_true(
    all(grps %in% paste(r2$g1, r2$g2))
  )
})




test_that("df_complement2 works as expected", {
  df1 <- data.frame(
    g1 = letters[c(1, 2, 2, 2, 2, 3, 4, 5, 6, 1)],
    g2 = LETTERS[c(1, 2, 3, 4, 5, 5, 5, 2, 1, 1)],
    da = 1:10
  )

  df2 <- data.frame(
    g1 = letters[c(1, 2, 3, 6, 7)],
    g2 = LETTERS[c(1, 2, 2, 2, 7)],
    da = 6:10,
    du = 100:104
  )

  df3 <- data.frame(
    g1 = letters[c(1, 2, 3, 6, 7)],
    g2 = LETTERS[c(1, 2, 4, 2, 7)],
    da = 6:10,
    du = 100:104,
    di = 1,
    dr = "a"
  )


  expect_error(
    df_complement2(df1, df2, c('g1', 'g2')),
    'not identical'
  )

  expect_silent(
    res <- df_complement2(df2, df3, c('g1', 'g2'))
  )
})
