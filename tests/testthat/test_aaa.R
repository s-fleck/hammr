context("Dummy")
library(magrittr)

library(doParallel)
cl <- makeCluster(8)
registerDoParallel(cl)


test_that("Dummy test", {
  expect_true(TRUE)
  suppressWarnings(library(data.table))
  data.table(NULL)
})

cat('\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n')
