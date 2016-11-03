context("extract_filename_from_path")

test_that("extracting filename from path works", {
  x <- c("C:/path/to/test.file.ext")

  expect_identical(extract_filename_from_path(x), "test.file.ext")
  expect_identical(extract_filename_from_path(x, ext = FALSE), "test.file")
})



