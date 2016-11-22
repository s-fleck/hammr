context('sqlgen_create_table')


cn1 <- LETTERS[1:18]
ct1 <- c('SMALLINT', 'INTEGER', 'INT', 'BIGINT', 'DECIMAL', 'NUMERIC', 'DECFLOAT',
         'REAL', 'DOUBLE', 'CHARACTER', 'CHARACTER(1)', 'VARCHAR(9)', 'CLOB(1)',
         'GRAPHIC(80)', 'VARGRAPHIC(80)', 'DBCLOB(80)', 'BLOB(80)', 'FAIL')


cn2 <- c('aba', NA, 'cdc')
ct2 <- c('integer', 'double', 'double')

cn3 <- c('aba', 'blubb', 'cdc')
ct3 <- c('integer', NA, 'double')

cn4 <- c('aba', NA, 'cdc')
ct4 <- c('integer', NA, 'double')


test_that("Dropping columns by name works.", {

  expect_silent(sqlgen_create_table('testtable', cn1[1:3], ct1[1:3]))
  expect_silent(sqlgen_create_table('testtable', cn4, ct4))

  expect_warning(expect_error(sqlgen_create_table('testtable', cn1, ct1)))
  expect_warning(sqlgen_create_table('testtable', cn3, ct3))

  expect_error(sqlgen_create_table('testtable', cn2, ct2))
  expect_warning(expect_error(sqlgen_create_table('testtable', cn1, ct1)))


})


