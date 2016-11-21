context('fetch_ftp')

testdat <- data.frame(
  a = factor(c(6,5,3,4,5)),
  b = factor(c('one', 'two', 'three', 'four', ' apple ')),
  c = c(' one ', ' two ', ' three ', ' four ', ' apple '),
  d = c(TRUE, TRUE, TRUE, FALSE, FALSE),
  e = c(1, 1, 1, 1, 1),
  f = c('moon', 'moon', 'moon', 'moon', 'moon'),
  g = c('TRUE', 'TRUE', 'TRUE', 'FALSE', 'FALSE'),
  h = c('3', '4', '5', '5', '5'),
  i = as.POSIXct(c('2015-01-01', '2015-01-05', '2015-05-04', '2015-12-01', '2015-04-13')),
  j = c('2015-01-01', '2015-01-05', '2015-05-04', '2015-12-01', '2015-04-13'),
  k = c('1', '1.5', '0.000000001', '100000000000', '99.1'),
  l = factor(c('1', '1.5', '0.000000001', '100000000000', '99.1')),
  m = factor(c('a', NA, NA, NA, NA)),
  n = factor(c(NA, NA, NA , NA, NA)),
  n2 = c('apple', 'applepie', 'moon', 'nomoon', 'moo'),
  stringsAsFactors = FALSE
)


test_that("Downloading files via ftp works.", {

  tfiles  <- c('dummyfile.blb', 'dumm2.blb')
  toutdir <- tempdir()
  ldir    <- tempdir()
  tcreds  <- list(user = "tuser", pw = "tpw")
  tserver <- 'dummy:server'
  tmode   <- 'ascii'

  expect_error(fetch_ftp(file, outdir, creds))

  tcmd <- tempfile()
  generate_ftp_command_file(fname = tcmd, creds = tcreds, mode = tmode, files = tfiles, local_dir = ldir)
  expect_identical(readLines(tcmd),
                   c("user tuser", "tpw", "cd .. ", "lcd \"/tmp/RtmpKFhPqO\"", "ascii ",
                     "get dummyfile.blb", "get dumm2.blb", "quit")
  )

  shellmock <- function(...){
    for(f in tfiles){
      writeLines('dummy', con = f)
    }
  }

  with_mock(
    fetch_ftp(.file = tfiles, .outdir = toutdir, .creds = tcreds, .server = tserver, .overwrite = FALSE, .mode = tmode),
    shell = shellmock)

})
