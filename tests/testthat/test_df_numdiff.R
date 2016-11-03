context('df_drop_cols')


dat1 <- data.frame(
  a = c('alpha', 'beta', 'ceta'),
  b = c(10, 12, 13),
  c = c(1L, 3L, 5L),
  d = factor(c('al', 'dl', 'zl')),
  stringsAsFactors = FALSE
)

dat2 <- data.frame(
  a = c('alpha', 'beta', 'ceta'),
  b = c(9, 15, 120),
  c = c(2L, 1L, 0L),
  d = factor(c('bl', 'ul', 'dl')),
  stringsAsFactors = FALSE
)

dat3 <- data.frame(
  a = c('alpha', 'beta', 'ceta'),
  z = c(9, 15, 12),
  c = c(2L, 1L, 0L),
  d = factor(c('bl', 'ul', 'dl')),
  stringsAsFactors = FALSE
)

dt1 <- as.data.table(dat1)
dt2 <- as.data.table(dat2)


test_that("Dropping columns by name works.", {

  expect_error(df_numdiff(dat1, dat3))
  expect_error(df_numdiff(dat1, dat2[ ,-1]))
  expect_error(df_numdiff(dat1, dat2[-1, ]))


  expect_silent(resdt <- df_numdiff(dt1,  dt2))
  expect_silent(resdf <- df_numdiff(dat1, dat2))


  expect_identical(class(resdt), c('data.table', 'data.frame'))
  expect_identical(class(resdf), c('data.frame'))
  expect_identical(as.data.frame(resdt), resdf)


  df_numdiff(dat1, dat2)

})
