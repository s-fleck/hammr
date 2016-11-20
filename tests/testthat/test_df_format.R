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

  expect_identical(rdf, tdf)
  expect_identical(rdt, rdf)
  expect_identical(rti, rdf)

  parenthesise <- function(x) paste0('(', trimws(x) , ')')

  tres <- df_format(tdf,
                    num_format   = list(digits = 3, big.mark = '.', decimal.mark = ','),
                    date_format  = list('%m/%d/%y'),
                    dtime_format = list('%m/%d/%y %H:%M:%S'),
                    col_format   = parenthesise)

  tres2 <- df_format(tdf,
                    num_format   = list(digits = 3, big.mark = '.', decimal.mark = ','),
                    date_format  = '%m/%d/%y',
                    dtime_format = '%m/%d/%y %H:%M:%S',
                    col_format   = parenthesise)

  eres <- data.frame(
    a = c("(alpha)", "(beta)", "(ceta)"),
    b = c("(1,23)",
          "(-1.124,00)", "(1,90)"),
    c = c("(1)", "(3)", "(5)"),
    d = c("(01/12/09)",
          "(01/12/09)", "(01/12/09)"),
    t = c(
      "(01/12/09 10:01:01)",
      "(01/12/09 23:01:03)",
      "(01/12/09 16:01:01)"
    ), stringsAsFactors = FALSE
  )

  expect_identical(tres, eres)

  expect_identical(
    df_format(tdf, dtime_format = '%m/%d/%y %H:%M:%S'),
    df_format(tdf, dtime_format = list(format = '%m/%d/%y %H:%M:%S'))
  )

  expect_identical(
    df_format(tdf, num_format = 2),
    df_format(tdf, num_format = list(digits = 2))
  )

})
