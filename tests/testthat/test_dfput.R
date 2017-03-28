context("dfput")


test_that("dfput works as expected", {
  tdat <- data.frame(
    x = 1:5,
    y = factor(c('dog', 'god', 'bob', 'rod', 'hog')),
    z = letters[1:5],
    stringsAsFactors = FALSE
  )

  tres_simple <- utils::capture.output(dfput(tdat))
  tres_char   <- utils::capture.output(dfput(tdat, factors = 'character'))
  tres_full   <- utils::capture.output(dfput(tdat, factors = 'full'))

  rs <- eval(parse(text = paste(tres_simple, collapse = '\n')))
  rf <- eval(parse(text = paste(tres_full, collapse = '\n')))
  rc <- eval(parse(text = paste(tres_char, collapse = '\n')))

  expect_identical(tdat, rs)
  expect_identical(tdat, rf)

  tdat$y <- as.character(tdat$y)
  expect_identical(tdat, rc)
})
