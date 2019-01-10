#' Generate SQL CREATE TABLE statements
#'
#' Creates SQL CREATE TABLE statements from a vector of column names and
#' a vector of column types
#'
#' @param table_name name of target sql table
#' @param col_names column names of target sql table
#' @param col_types column types of target sql table
#' @param col_options column options of target sql table (for example `NOT
#'   NULL`)
#' @param dialect optional: checks if input is valid for target SQL dialect.
#'   Currenlty the only supported value is `"db2"`.
#'
#' @return a `CREATE TABLE` statement as a text string
#' @export
#'
#' @examples
#' #'
#' sqlgen_create_table(
#'   "example.table",
#'   c("numbers", "animals"),
#'   c("integer", "varchar(8)"),
#'   c("NOT NULL", "")
#' )
#'
#' # [1] "CREATE TABLE example.table (numbers INTEGER NOT NULL, animals VARCHAR(x))"
#'
sqlgen_create_table <- function(
  table_name,
  col_names,
  col_types,
  col_options = NULL,
  dialect = NULL
){
  assert_namespace("stringi")
  .Deprecated(msg = "Bitte tabde verwenden")

  # preconditions
    assert_that(is_scalar_character(table_name))
    assert_that(is.character(col_names))
    assert_that(is.character(col_types))
    assert_that(identical(length(col_names), length(col_types)))

    assert_that(all(
      is.na(col_names) == FALSE |
      is.na(col_names) == is.na(col_types)
    ))

    assert_that(
      is.null(col_options) ||
      length(identical(col_options, length(col_names)))
    )

    col_types  <- toupper(col_types)


  # process input
    empty_cols <- is.na(col_names) && is.na(col_types)
    col_names  <- col_names[!empty_cols]
    col_types  <- col_types[!empty_cols]

    if(any(is.na(col_types))){
      warning('Skipping col defintions where col_type is NA')

      col_names <- col_names[!is.na(col_types)]
      col_types <- col_types[!is.na(col_types)]
    }

    if(is.null(col_options)){
      col_options <- rep('', length(col_names))
    }
      col_options[is.na(col_options)] <- ''


  # check processed col_types
    assert_that(check_sql_types(col_types, dialect = dialect))


  cols <- paste0(
    trimws(paste0(col_names, ' ', col_types, ' ', col_options)),
    collapse = ', '
  )

  sprintf('CREATE TABLE %s (%s)', table_name, cols)
}



check_sql_types <- function(col_types, dialect){
  if(is.null(dialect)){
    return(TRUE)
  }

  dialect <- tolower(dialect)

  switch(
    dialect,
    'db2' = check_sql_types_db2(col_types)
  )
}



check_sql_types_db2 <- function(col_types){
  valid_col_types <- c(
    'SMALLINT',
    'INTEGER', 'INT',
    'BIGINT',
    'DECIMAL', 'NUMERIC',
    'DECFLOAT',
    'REAL',
    'DOUBLE',
    'CHARACTER',
    'CHARACTER\\([1-9]{1,3}\\)',
    'VARCHAR\\([1-9][0:9]{0,2}\\)',
    'CLOB\\([1-9][0:9]{0,2}\\)',
    'GRAPHIC\\([1-9][0:9]{0,2}\\)',
    'VARGRAPHIC\\([1-9][0:9]{0,2}\\)',
    'DBCLOB\\([1-9][0:9]{0,2}\\)',
    'BLOB\\([1-9][0:9]{0,2}\\)',
    'DATE',
    'TIME',
    'TIMESTAMP'
  )

  res <- vector('list', length(col_types))
  names(res) <- col_types

  for(col_type in col_types){
    res[[col_type]] <- any(
      stringi::stri_detect(col_type, regex = valid_col_types)
    )
  }


  res
}
