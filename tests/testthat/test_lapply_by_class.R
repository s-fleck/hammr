context('lapply_by_class')

tdf <- data.frame(
  a = c('alpha', 'beta', 'ceta'),
  b = c(1.230,-1124.0, 1.90),
  c = c(190L, 3111111L, 5L),
  d = as.Date(c('2009-01-12', '2009-01-12', '2009-01-12')),
  t = as.POSIXct(c('2009-01-12 10:01:01', '2009-01-12 23:01:03', '2009-01-12 16:01:01'), format = "%Y-%m-%d %H:%M:%S"),
  stringsAsFactors = FALSE)


test_that("lapply_by_class works.", {

  parenthesise <- function(x) paste0('(', trimws(x) , ')')
  res <- lapply_if_class(tdf, parenthesise, 'character')

  expect_identical(res$a, c("(alpha)", "(beta)", "(ceta)"))
  expect_identical(res$b, tdf$b)
  expect_identical(res$c, tdf$c)
  expect_identical(res$d, tdf$d)
  expect_identical(res$t, tdf$t)

})

