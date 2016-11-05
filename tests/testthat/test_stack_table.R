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


expect_error(stack_table(tdat1, tdat2))
stack_table(tdat1, tdat2, rem_ext = '_xt')
