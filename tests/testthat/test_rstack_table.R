context("Rstack_table")

# setwd(system.file('tests', 'testthat', package = 'hammr'))

line1 <- readRDS('test_data/rstack_table/line1.rds')
line2 <- readRDS('test_data/rstack_table/line2.rds')


test_that("Greater / Less than operations work for quarters.", {

  expect_error(rstack_table(line1, line2))
  rstack_table(line1, line2, format = 'latex', remove_ext = '_cv')
  rstack_table(line1, line2, format = 'excel', remove_ext = '_cv')
  rstack_table(line1, line2, format = 'excel', remove_ext = '_cv', insert_empty_line = TRUE)

})


test_that("Printing Rstack tables as latex works"){
  dat <- rstack_table(line1, line2, format = 'latex', remove_ext = '_cv')
  print(dat, format = 'latex')
}



test_that("Printing Rstack tables as latex works"){
  dat <- rstack_table(line1, line2, format = 'xlsx', remove_ext = '_cv')
  print(dat, format = 'latex')
}
