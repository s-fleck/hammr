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


test_that('stacking tables by row works', {
  # Creating stack tables
    expect_error(stack_table(tdat1, tdat2))
    expect_silent(st1 <- stack_table(tdat1, tdat2, rem_ext = '_xt'))
    expect_silent(st2 <- stack_table(tdat1, tdat3, rem_ext = '_xt'))

  # Create row stacked data.table
    expect_error(stack_rows(tdat1))
    res1 <- stack_rows(st1)
    res2 <- stack_rows(st2)

  # Create a col_stacked data.table
    expect_error(stack_rows(tdat1))
    res1 <- stack_cols(st1)
    res2 <- stack_rows(st2)

    stack_rows(st1)
    stack_rows_tex(st1, insert_empty_row = TRUE)
    stack_rows_tex(st1, insert_empty_row = FALSE)
    print_tex(st1, stack_method = 'row')

    stack_cols(st1)
    stack_cols_tex(st1)
    print_tex(st1, stack_method = 'col')

    expect_identical(lapply(res1, class), lapply(tdat1, class))
    expect_identical(as.character(lapply(res2, class)), as.character(lapply(tdat3, class)))

  # Create row stacked latex table
    print_tex(st1)
})

test_that('stacking tables by col', {
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

  # Create row stacked latex table
  print_tex(st1)
})

