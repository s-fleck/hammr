context('stat_assertions')

test_that("calc_urs_check_digit: checking of urs checksum works", {
  x <- c("Z008W3509", "Z004L4056", "Z000A9205", "Z002E173Y", "Z005I796U", "Z005I796U", "Z001U954W", "Z000A212W", "Z001Z960U", "Z006C660J", "Z007U042V",
         "Z005M317G", "Z090V175H", "Z008M7137", "Z009L496E", "Z011V721N", "Z011A739M", "Z020S621A", "Z018M5022", "Z010J6414", "Z005M316I", "Z021I865W",
         "Z002B329Z", "Z002E005R", "Z012Y470N", "Z093B129A", "Z093U465S", "Z093Z9216", "Z093Z9224", "Z091Q4213", "Z020A2423", "Z019E997K")

  test_cases <- substring(x, 1, 8)
  should     <- substring(x, 9, 9)

  is <- calc_urs_check_digit(test_cases)

  expect_identical(should, is)

})
