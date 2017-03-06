context("df_compare_by")

dat1 <- data.frame(
  a  = c('alpha', 'beta', 'ceta', 'beta', 'beta', 'alpha'),
  a2 = c('aleph', 'beth', 'beth', 'alpeh', 'aleph', 'beth'),
  b  = c(10,-10, 90, 60, 62, 10),
  c  = c(1L, 3L, 5L, 6L, -1L, -2L),
  d  = factor(c('al', 'dl', 'zl', 'bl', 'bl', 'ba')),
  stringsAsFactors = FALSE
)

dat2 <- data.frame(
  a2 = c('aleph', 'aleph', 'beth', 'beth'),
  a  = c('alpha', 'beta', 'ceta', 'delta'),
  b  = c(10, 20, 100, 9),
  c  = c(2L, 1L, 0L, 10L),
  d  = factor(c('bl', 'ul', 'dl', 'br')),
  stringsAsFactors = FALSE
)


test_that("df_compare_by works as expected", {

  df_compare_by(
    dat1,
    dat2,
    by = c('a', 'a2'),
    fun = `-`
  )


})
