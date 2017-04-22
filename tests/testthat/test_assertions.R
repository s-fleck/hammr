context("Assertions")

tdat1 <- data.frame(
  x = c('a', 'b', 'c'),
  y = factor(c('r', 'u', 'b')),
  z = c(1, 2, 3),
  stringsAsFactors = FALSE
)


test_that("Infix class checking workss.", {
  #* @testing %is_class%
  #* @testing %assert_class%
  expect_warning('blah' %is_class% 'character')
  expect_silent('blah' %assert_class% 'character')
})


# is_col_classes
test_that("is_col_classes: mass checking of column types works.", {

  testfuns <- function(dat){
    true_classes   <- list(x = 'character', y = 'factor', z = 'numeric')
    false_classes  <- list(x = 'character', y = 'factor', z = 'character')
    error_classes  <- c('character', 'factor', 'numeric')
    error_classes2 <- list(x = 'character', y = 'factor', z = 'numeric', u = 'integer')

    expect_true(is_col_classes(dat, true_classes))
    expect_warning(expect_false(is_col_classes(dat, false_classes)))

    expect_error(is_col_classes(dat, error_classes))
    expect_true(is_col_classes(dat, true_classes[1:2], method = 'all'))

    expect_true(is_col_classes(dat, true_classes[1:2], method = 'all'))
    expect_true(is_col_classes(dat, true_classes[1:2], method = 'any'))
    expect_error(is_col_classes(dat, true_classes[1:2], method = 'identical'))

    expect_error(is_col_classes(dat, error_classes2, method = 'all'))
    expect_true(is_col_classes(dat, error_classes2, method = 'any'))
    expect_error(is_col_classes(dat, error_classes2, method = 'identical'))

    expect_error(is_col_classes(dat, error_classes2, method = 'all'))
    expect_error(is_col_classes(dat, classes = 1L))
    expect_error(is_col_classes(dat, classes = list()))
  }

  testfuns(tdat1)
  testfuns(as.list(tdat1))
})



