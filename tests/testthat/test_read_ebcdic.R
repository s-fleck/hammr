context("Read EBCDIC")

testdat <- data.frame(
  a = as.raw(c(0x13, 0x48, 0x80, 0x5c)),
  b = as.raw(c(0x13, 0x48, 0x80, 0x5d)),
  c = as.raw(c(0x13, 0x48, 0x80, 0x01)),
  d = c(13, 48, 80, 01)
)

test_that("Test parsing of packed decimal numbers.", {

  # Postive numbers
  # expect_equal(parse_packed_decimal(testdat$a), 1348805)
  expect_equal(parse_packed_decimal(testdat$a, d = 1L), 134880.5)
  expect_equal(parse_packed_decimal(testdat$a, d = 2),  13488.05)

  # Negative numbers
  expect_equal(parse_packed_decimal(testdat$b, d = 1),  -134880.5)
  expect_equal(parse_packed_decimal(testdat$b, d = 2),  -13488.05)

  # No sign nibble present
  expect_identical(parse_packed_decimal(testdat$c, d = 3, psign = NULL, nsign = NULL), 13488.001)

  # Expected error messages
  expect_error(parse_packed_decimal(testdat$a, d = c(1, 2)))
  expect_error(parse_packed_decimal(testdat$a, d = 1, psign = c('d', 'e'), nsign = c('c', 'd')))
  expect_error(parse_packed_decimal(testdat$c, d = 1))
  expect_error(parse_packed_decimal(testdat$c, d = 1))
  expect_error(parse_packed_decimal(testdat$c, d = 1))
})



test_that("Test parsing of packed decimal numbers.", {

  expect_identical(parse_raw(testdat$a, format = 'pd.4'), 134.8805)
  expect_error(parse_raw(testdat$a, format = 'pd9.'))
  expect_error(parse_raw(testdat$a, format = 'pd6.4'))

})

