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
  expect_silent('blah' %assert_class% 'character')
})
