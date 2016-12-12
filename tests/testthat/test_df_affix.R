context('df_affix')

testdat <- data.frame(
  a = c(1, 2),
  b = c('a', 'b'),
  stringsAsFactors = FALSE
)


test_that("Dropping columns by name works.", {
  r1 <- data.frame(
    a = c('p1s', 'p2s'),
    b = c('pas', 'pbs'),
    stringsAsFactors = FALSE
  )

  r2 <- data.frame(
    a = c('(1)', '(2)'),
    b = c('(a)', '(b)'),
    stringsAsFactors = FALSE
  )

  expect_identical(df_affix(testdat, 'p', 's'), r1)
  expect_identical(df_parenthesis(testdat), r2)

})
