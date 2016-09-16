context("Read EBCDIC")

# setwd(system.file('tests', 'testthat', package = 'hammr'))
# hammr::fetch_ftp('GVK.FAHRTEN.J2015', .creds = creds, .mode = 'bin', .overwrite = TRUE, .outdir = 'D:/_datasets/sgvk')
# should <- data.table::fread('P:/Verkehr/Projekte/Fleck/R/SGVK/inst/extdata/sample_data/fahrt2015_kmneu.csv')
# infile <- "D:/_datasets/sgvk/GVK.FAHRTEN.J2015_bin"  # encoding="IBM500",
# insize <- file.size("D:/_datasets/sgvk/GVK.FAHRTEN.J2015_bin")
# datBin   <- readBin(file(infile, 'rb'), 'raw', n = insize)
# datAscii <- readLines(file(file_path, raw = TRUE, encoding = "IBM500"), n = 10, skipNul = TRUE)


testdat <- data.frame(
  a = as.raw(c(0x13, 0x48, 0x80, 0x5c)),
  b = as.raw(c(0x13, 0x48, 0x80, 0x5d)),
  c = as.raw(c(0x13, 0x48, 0x80, 0x01)),
  d = c(13, 48, 80, 01)
)


testdat2  <- readRDS(file.path('test_data', 'parse_ebcdic', 'testdata.rds'))
tsplt     <- split_raw_records(testdat2, lsep = as.raw(c(0xc7, 0xe5, 0xd2)))



fields_strukt <- list(
  list(name = 'jahr',  start = 1L,  end = 2L,  parser = 'character'),
  list(name = 'monat', start = 3L,  end = 4L,  parser = 'character'),
  list(name = 'bermo', start = 5L,  end = 6L,  parser = 'character'),
  list(name = 'bearb', start = 10L, end = 11L, parser = 'character'),
  list(name = 'kzs'  , start = 18L, end = 27L, parser = 'character'),
  list(name = 'berw' , start = 27L, end = 28L, parser = 'character'),

  # Schicht information
  pfield('bdl',   29),
  pfield('nlkl',  30),
  pfield('zeit',  31, 32),
  pfield('hochr', 33, 36, 'pd.4'),
  pfield('wf',    37),

  pfield('anz', 107, 108, 'bin')
)



test_that('splitting a raw vector into records works', {
  r1 <- tsplt[[1]]
  r2 <- tsplt[[2]]

  expect_identical(r1[1:5],                       as.raw(c(0xF1, 0xF5, 0xF0, 0xF1, 0xF0)))
  expect_identical(r1[(length(r1)-4):length(r1)], as.raw(c(0x40, 0x40, 0x40, 0xF1, 0x40)))

  expect_identical(r2[1:5],                       as.raw(c(0xF1, 0xF5, 0xF0, 0xF1, 0xF0)))
  expect_identical(r2[(length(r2)-4):length(r2)], as.raw(c(0x40, 0x40, 0x40, 0xF1, 0xF9)))
})


test_that('parsing a raw line works', {
  rline <- tsplt[[2]]

  x <- parse_ebcdic_line(rline, fields = fields_strukt)








})

test_that("EBCDIC field parser works", {


})

test_that("parsing of packed decimal numbers works.", {

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


test_that("Matching a vector to a vector works", {
  rline <- tsplt[[1]]

  x    <- as.raw(c(0xf0, 0xf0, 0xe7))
  m    <- match_vector(rline, x)

  for(i in m){
    expect_true(identical(    rline[i:(i+2)], x))
  }

  rlineChar <- paste0(rline, collapse = '')
  xchar       <- paste0(x, collapse = '')
  m_char      <- as.integer(stringr::str_locate_all(rlineChar, xchar)[[1]][,1] / 2 + 0.5)

  expect_identical(m, m_char)
})






test_that("Test parsing of packed decimal numbers.", {

  expect_identical(parse_ebcdic(testdat$a, parser = 'pd.4'), 134.8805)
  expect_error(parse_ebcdic(testdat$a,     parser = 'pd9.'))
  expect_error(parse_ebcdic(testdat$a,     parser = 'pd6.4'))

})

