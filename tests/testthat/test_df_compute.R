context('df_compute')

#* @testing df_compute

dat1 <- data.frame(
  a = c('alpha', 'beta', 'ceta'),
  b = c(10,-10, 90),
  c = c(1L, 3L, 5L),
  d = factor(c('al', 'dl', 'zl')),
  stringsAsFactors = FALSE
)

dat2 <- data.frame(
  a = c('alpha', 'beta', 'ceta'),
  b = c(10, 20, 100),
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

dt1 <- data.table::as.data.table(dat1)
dt2 <- data.table::as.data.table(dat2)


test_that("df_compute works", {

  expect_error(df_ndiff(dat1, dat3))
  expect_error(df_ndiff(dat1, dat2[ ,-1]))
  expect_error(df_ndiff(dat1, dat2[-1, ]))

  expect_silent(resdt <- df_ndiff(dt1,  dt2))
  expect_silent(resdf <- df_ndiff(dat1, dat2))

  expect_identical(class(resdt), c('data.table', 'data.frame'))
  expect_identical(class(resdf), c('data.frame'))
  expect_identical(as.data.frame(resdt), resdf)


  res1 <- df_ndiff(dat1, dat2, coltypes = 'numeric')
  expect_identical(res1$a, dat1$a)
  expect_identical(res1$b, c(0, -30, -10))
  expect_identical(res1$c, dat1$c)
  expect_identical(res1$d, dat1$d)


  res2 <- df_ndiff(dat1, dat2, coltypes = c('integer', 'numeric'))
  expect_identical(res2$a, dat1$a)
  expect_identical(res2$b, c( 0, -30, -10))
  expect_identical(res2$c, c(-1L, 2L,   5L))
  expect_identical(res2$d, dat1$d)


  res3 <- df_ndiff(dat1, dat2, coltypes = c('integer'))
  expect_identical(res3$a, dat1$a)
  expect_identical(res3$b, dat1$b)
  expect_identical(res3$c, c(-1L, 2L, 5L))
  expect_identical(res3$d, dat1$d)

  df_pdiff(dat2, dat1)
})


context("df_compute with id_vars")

test_that("df_compute with id_vars", {
  dat1 <- data.frame(
    a  = c('alpha', 'beta', 'ceta'),
    a2 = c('a', 'b', 'c'),
    b  = c(10,-10, 90),
    c  = c(1L, 3L, 5L),
    d  = factor(c('al', 'dl', 'zl')),
    stringsAsFactors = FALSE
  )

  dat2 <- data.frame(
    a = c('ceta', 'alpha', 'beta'),
    a2  = c('c', 'a', 'b'),
    b  = c(10, 20, 100),
    c  = c(2L, 1L, 0L),
    d  = factor(c('bl', 'ul', 'dl')),
    stringsAsFactors = FALSE
  )

  eres <- data.frame(
    a = c("alpha", "beta", "ceta"),
    a2 = c("a", "b", "c"),
    b = c(-10, -110, 80),
    c = c(1L, 3L, 5L),
    d = factor(c("al", "dl", "zl")),
    stringsAsFactors = FALSE
  )

  tres <- df_compute(
    dat1,
    dat2,
    id_vars = c('a', 'a2'),
    fun = `-`
  )

  expect_identical(
    eres, tres
  )


})

