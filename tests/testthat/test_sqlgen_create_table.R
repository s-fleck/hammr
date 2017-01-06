context('sqlgen_create_table')


cn1 <- LETTERS[1:18]
ct1 <- c('SMALLINT', 'INTEGER', 'INT', 'BIGINT', 'DECIMAL', 'NUMERIC', 'DECFLOAT',
         'REAL', 'DOUBLE', 'CHARACTER', 'CHARACTER(1)', 'VARCHAR(9)', 'CLOB(1)',
         'GRAPHIC(80)', 'VARGRAPHIC(80)', 'DBCLOB(80)', 'BLOB(80)', 'FAIL')
co1 <- c(rep('NOT NULL', length(ct1)))


cn2 <- c('aba', NA, 'cdc')
ct2 <- c('integer', 'double', 'double')

cn3 <- c('aba', 'blubb', 'cdc')
ct3 <- c('integer', NA, 'double')

cn4 <- c('aba', NA, 'cdc')
ct4 <- c('integer', NA, 'double')



test_that("check_sql_types_db2 works.", {

  expect_warning(expect_false(check_sql_types_db2(ct1)))
  expect_true(check_sql_types_db2(ct1[-length(ct1)]))


  expect_warning(
    expect_false(check_sql_types_db2(ct1))
  )

  expect_warning(
    expect_false(check_sql_types(ct1, dialect = 'Db2'))
  )

  expect_true(check_sql_types_db2(ct1[-length(ct1)]))
  expect_true(check_sql_types(ct1[-length(ct1)], dialect = 'dB2'))
})


test_that("sqlgen_create_table works.", {
  expect_silent(sqlgen_create_table('testtable', cn1[1:3], ct1[1:3]))
  expect_silent(sqlgen_create_table('testtable', cn4, ct4))

  expect_warning(
    expect_error(
      sqlgen_create_table('testtable', cn1, ct1, dialect = 'DB2')
    )
  )

  expect_warning(sqlgen_create_table('testtable', cn3, ct3))
  expect_error(sqlgen_create_table('testtable', cn2, ct2))
  expect_warning(expect_error(sqlgen_create_table('testtable', cn1, ct1, dialect = 'DB2')))
})


