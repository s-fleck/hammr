context("Read EBCDIC")
#
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

problem_line_1 <- as.raw(c(0xf1, 0xf5, 0xf0, 0xf1, 0xf0,
                                                  0xf1, 0x40, 0x40, 0x40, 0xf8, 0xf4, 0x40, 0x40, 0x40, 0x40, 0x40,
                                                  0x40, 0xe2, 0xf0, 0xf0, 0xf1, 0xd7, 0xf5, 0xf0, 0xf9, 0xf1, 0xf0,
                                                  0xf1, 0xf5, 0xf2, 0xf0, 0xf1, 0x13, 0x11, 0x00, 0x0c, 0xf2, 0x40,
                                                  0x40, 0x40, 0xf4, 0xf9, 0xf4, 0xf1, 0x40, 0x40, 0x40, 0x40, 0x40,
                                                  0xf1, 0xf1, 0xf0, 0xf0, 0xe2, 0xd3, 0x40, 0xf9, 0xf8, 0xf3, 0xd4,
                                                  0xc6, 0xf1, 0xf1, 0xf3, 0xf0, 0xf3, 0xf2, 0xf0, 0xf0, 0xf0, 0xf0,
                                                  0xf0, 0xf8, 0xf0, 0xf0, 0xf0, 0xf0, 0xf4, 0xf0, 0xf3, 0xf9, 0xf8,
                                                  0xf6, 0xf8, 0xf0, 0xf3, 0xf9, 0xf9, 0xf5, 0xf4, 0x40, 0xf0, 0xf0,
                                                  0xf0, 0xf0, 0xf7, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf7,
                                                  0xf1, 0xf6, 0x00, 0x01, 0xf0, 0xf5, 0x40, 0x40, 0x40, 0x40, 0x40,
                                                  0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40,
                                                  0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40,
                                                  0x40, 0x40, 0x40, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xe7, 0x40,
                                                  0xf1, 0xf0, 0xf0, 0x40, 0x40, 0x40, 0xf5, 0xf0, 0xf2, 0xf0, 0xf1,
                                                  0xf0, 0xf0, 0xf0, 0xf5, 0xf0, 0xf1, 0xf0, 0xf1, 0xf1, 0xf0, 0xf0,
                                                  0x40, 0x40, 0x40, 0xf5, 0xf0, 0xf2, 0xf0, 0xf1, 0xf0, 0xf0, 0xf0,
                                                  0xf5, 0xf0, 0xf1, 0xf0, 0xf1, 0xf0, 0xf0, 0xf0, 0xf7, 0xf0, 0xf0,
                                                  0xf0, 0xf0, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40,
                                                  0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40,
                                                  0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40,
                                                  0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40,
                                                  0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0xf1, 0xf6, 0x40,
                                                  0x40, 0x40, 0x40, 0x40, 0x40, 0xf1, 0x40, 0xc7, 0xe5, 0xd2))



trips_ebcdic_fields_0114 <- list(
  pfield('jahr',     1,  2),
  pfield('monat',    3,  4),
  pfield('bermo',    5,  6),
  pfield('infev',    7),
  pfield('ausfall',  8),
  pfield('fil',      9),
  pfield('bearb',   10, 11),
  pfield('hf_neu',  12, 16),
  pfield('frei',    17),
  pfield('kzs',     18, 26),
  pfield('berw',    27, 28),

  # Schicht information
  pfield('bdl',   29),
  pfield('nlkl',  30),
  pfield('zeit',  31, 32),
  pfield('hochr', 33, 36, 'pd.4'),    # Zwischen 2014 und 2016 stand hier hochrechnungsgewicht als pd.4
  pfield('fws',   37),
  pfield('gru',   38),
  pfield('geow',  39),

  pfield('web',     40),
  pfield('nace',    41, 44),
  pfield('lkwzug',  50),
  pfield('natkfz',  51, 52),
  pfield('kfzkenn', 54, 61),
  pfield('fuhrwe',  62),
  pfield('erstzul', 63, 64),
  pfield('geskg',   65, 70),
  pfield('nutzkg',  71, 76),
  pfield('achsen',  77, 78),
  pfield('kmbeg',   79, 84),
  pfield('kmend',   85, 90),
  pfield('strukt',  91),
  pfield('trans',   92),
  pfield('fkminl',  93, 96),
  pfield('fkmaus',  97, 100),
  pfield('fkmges', 101, 104),
  pfield('fva',    105, 106),
  pfield('anz',    107, 108, 'bin')
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
  expect_identical(r1[(length(r1)-7):length(r1)], as.raw(c(0x40, 0x40, 0x40, 0xF1, 0x40, 0xc7, 0xe5, 0xd2)))

  expect_identical(r2[1:5],                       as.raw(c(0xF1, 0xF5, 0xF0, 0xF1, 0xF0)))
  expect_identical(r2[(length(r2)-7):length(r2)], as.raw(c(0x40, 0x40, 0x40, 0xF1, 0xF9, 0xc7, 0xe5, 0xd2)))
})


test_that('parsing a raw line works', {
  rline <- tsplt[[2]]

  x <- parse_ebcdic_line(rline, fields = fields_strukt)
  x <- hammr:::parse_ebcdic_line(problem_line_1, fields = trips_ebcdic_fields_0114)


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
  m    <- which(match_vector(rline, x))

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

