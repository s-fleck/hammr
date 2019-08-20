context("duplicated_combinations")


test_that("is_duplicated_combinations works as expected", {
  
  tdat <- data.frame(
    a = LETTERS[c(1:3, 5, 7)],
    b = LETTERS[c(3:1, 5, 6)],
    c = 1:5
  )
  
  eres <- c(FALSE, FALSE, TRUE, FALSE, FALSE)
  eres2 <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
  
  expect_identical(
    duplicated_combinations(list(tdat$a, tdat$b)),
    eres)
  expect_identical(
    duplicated_combinations(list(tdat$a, tdat$b), fromLast = TRUE), 
    eres2)
  
  
  expect_identical(
    duplicated_combinations(tdat[, 1:2]),
    eres)
  expect_identical(
    duplicated_combinations(tdat[, 1:2], fromLast = TRUE), 
    eres2)
  
  
  expect_identical(
    duplicated_combinations(as.matrix(tdat[, 1:2])),
    eres)
  expect_identical(
    duplicated_combinations(as.matrix(tdat[, 1:2]), fromLast = TRUE), 
    eres2)
  
})
