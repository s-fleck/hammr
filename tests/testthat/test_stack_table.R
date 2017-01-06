context('stack_table')

tdat1 <- data.frame(
  numbers = c(1.434, 190.3, 228.311, 5.210, 4321543),
  animals = c('dog', 'cat', 'camel', 'pig', 'mouse'),
  factors = factor(c('rain', 'grain', 'stain', 'pain', 'main')),
  ints    = c(10L, 20L, 40L, 30L, 50L),
  stringsAsFactors = FALSE
)

tdat2 <- data.frame(
  numbers_xt = c(1, 290, 0.311, 0.210, 1000),
  animals_xt = c('god', 'tac', 'lemac', 'gip', 'esuom'),
  factors_xt = factor(c('tractor', 'hector', 'andrew', 'milli', 'vanilli')),
  ints    = c(5L, 5L, 10L, 30L, 25L),
  stringsAsFactors = FALSE
)


tdat3 <- data.frame(
  numbers_xt = factor(c(1, 290, 0.311, 0.210, 1000)),
  animals_xt = factor(c('god', 'tac', 'lemac', 'gip', 'esuom')),
  factors_xt = factor(c('tractor', 'hector', 'andrew', 'milli', 'vanilli')),
  ints       = c('god', 'tac', 'lemac', 'gip', 'esuom'),
  stringsAsFactors = FALSE
)


test_that('stack_table: stacking tables by row works', {
  #* @testing stack_rows
  #* @testing as.data.table.Stack_table

  # Creating stack tables
    expect_error(stack_table(tdat1, tdat2))
    expect_silent(st1 <- stack_table(tdat1, tdat2, rem_ext = '_xt'))
    expect_silent(st2 <- stack_table(tdat1, tdat3, rem_ext = '_xt'))

  # Create row stacked data.table
    expect_error(stack_rows(tdat1))
    res1 <- stack_rows(st1)
    res2 <- stack_rows(st2)

    expect_identical(lapply(res1, class), lapply(tdat1, class))
    expect_identical(as.character(lapply(res2, class)), as.character(lapply(tdat3, class)))

  # Inserting blank rows works
    res1 <- as.data.table(st1, insert_blank_row = TRUE)
    expect_warning(as.data.table(st1, stack_method = 'col', insert_blank_row = TRUE))

    sel <- seq(3, nrow(res1), 3)
    expect_true(all(rowSums(res1[sel] == '') == ncol(res1)))
    expect_false(any(rowSums(res1[-sel] == '') == ncol(res1)))

})


test_that('printing as latex works', {
  #* @testing stack_rows_tex
  #* @testing print_tex

  expect_silent(st1 <- stack_table(tdat1, tdat2, rem_ext = '_xt'))
  expect_silent(st2 <- stack_table(tdat1, tdat3, rem_ext = '_xt'))

  res1 <- stack_rows(st1)
  res2 <- stack_rows(st2)

  expect_silent(stack_rows_tex(st1, insert_blank_row = TRUE))
  expect_silent(stack_rows_tex(st1, insert_blank_row = FALSE))
  expect_output(print_tex(st1, stack_method = 'row'))
  expect_output(print_tex(st2))
})



test_that('exporting as xlsx works', {
  expect_silent(st1 <- stack_table(tdat1, tdat2, rem_ext = '_xt'))
  # save_xlsx.StackTable(st1, '/home/hoelk/blah.xlsx', overwrite = TRUE)
  # save_xlsx.StackTable(st1, '/home/hoelk/blah.xlsx', overwrite = TRUE, startRow = 10)
  # save_xlsx.StackTable(st1, '/home/hoelk/blah.xlsx', overwrite = TRUE, xy = c(6, 10))
  # save_xlsx.StackTable(st1, '/home/hoelk/blah.xlsx', overwrite = TRUE, sep_height = 24, xy = c(6, 10))
  # save_xlsx.StackTable(st1, '/home/hoelk/blah.xlsx', overwrite = TRUE, sep_height = 24, xy = c(6, 10), insert_blank_row = TRUE)
})
