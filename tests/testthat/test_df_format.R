context('df_format')



tdf <- data.frame(
  a = c('alpha', 'beta', 'ceta'),
  b = c(1.230,-1124.0, 1.90),
  c = c(1L, 3L, 5L),
  d = as.Date(c('2009-01-12', '2009-01-12', '2009-01-12')),
  t = as.POSIXct(c('2009-01-12 10:01:01', '2009-01-12 23:01:03', '2009-01-12 16:01:01'), "%Y-%m-%d %H:%M:%S"),
  stringsAsFactors = FALSE)

tdt <- data.table::as.data.table(tdf)

tti <- tibble::as_tibble(tdf)



test_that("classes get preserved.", {

  rdf <- df_format(tdf)
  rdt <- df_format(tdt)
  rti <- df_format(tti)

  expect_identical(class(rdf), 'data.frame')
  expect_identical(class(rdt), c('data.table', 'data.frame'))

  df_format(tdf, num_formatC = list(digits = 1, format = 'f'))


})
