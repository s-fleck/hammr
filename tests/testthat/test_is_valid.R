context("test_is_valid")


test_that("test_is_valid works as expected", {

  testx <- 1
  class(testx) <- c('test', 'rest')

  is_valid.test <- function(testx) return(FALSE)

  tfun <- function(xtt){
    assert_valid((xtt))
  }

  expect_error(tfun(testx))

})
