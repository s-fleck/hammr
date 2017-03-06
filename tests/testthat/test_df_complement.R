context("df_complement")


df1 <- data.frame(
  g1 = letters[c(1:5)],
  da = 1:5
)

df2 <- data.frame(
  g1 = letters[c(1, 2, 3, 6, 7)],
  da = 6:10
)


test_that("df_complement works as expected", {

  df_complement(df1, df2$g1, 'g1')
  df_complement(df2, df1$g1, 'g1')

})
