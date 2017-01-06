context('df_format')

tdf <- data.frame(
  a = c('alpha', 'beta', 'ceta'),
  b = c(1.230,-1124.0, 1.90),
  c = c(190L, 3111111L, 5L),
  d = as.Date(c('2009-01-12', '2009-01-12', '2009-01-12')),
  t = as.POSIXct(c('2009-01-12 10:01:01', '2009-01-12 23:01:03', '2009-01-12 16:01:01'), format = "%Y-%m-%d %H:%M:%S"),
  stringsAsFactors = FALSE)

tdt <- data.table::as.data.table(tdf)

tti <- tibble::as_tibble(tdf)

test_that("df_format yields expected col classes", {

  tres <- df_format(tdf, numeric = c(digits = 3), 'integer' = list(big.mark = '-'))
  expect_identical(class(tres), 'data.frame')
  expect_warning(df_format(tdf, numeric = c(digits = 3), 'integer' = list(big.mark = '-'), 'blubb' = 3))

})


test_that("df_format works.", {

  tres <- df_format(tdf,
                    numeric   = list(digits = 3, big.mark = '.', decimal.mark = ','),
                    integer   = list(digits = 3, big.mark = '.', decimal.mark = ','),
                    Date      = list('%m/%d/%y'),
                    POSIXct   = list('%m/%d/%y %H:%M:%S'))

  tres2 <- df_format(tdf,
                     numeric  = list(digits = 3, big.mark = '.', decimal.mark = ','),
                     integer  = list(digits = 3, big.mark = '.', decimal.mark = ','),
                     Date     = '%m/%d/%y',
                     POSIXct  = '%m/%d/%y %H:%M:%S')

  eres <- data.frame(
    a = c("alpha", "beta", "ceta"),
    b = c("     1,23", "-1.124,00", "     1,90"),
    c = c("      190", "3.111.111", "        5"),
    d = c("01/12/09", "01/12/09", "01/12/09"),
    t = c("01/12/09 10:01:01", "01/12/09 23:01:03", "01/12/09 16:01:01"), stringsAsFactors = FALSE
  )

  expect_identical(tres, eres)
  expect_identical(tres, tres2)

  tres3 <- df_format_num(tdf, digits = 3, big.mark = '.', decimal.mark = ',')

  expect_identical(tres3[[1]], eres[[1]])
  expect_identical(tres3[[2]], eres[[2]])
  expect_identical(tres3[[3]], eres[[3]])
  expect_identical(tres3[[4]], tdf[[4]])
  expect_identical(tres3[[5]], tdf[[5]])
})
