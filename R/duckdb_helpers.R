.get_duckdb_con <- function(dbfile = NULL) {

  if (!is.null(dbfile)) {

    dbdir <- dbfile

  } else {

    dbdir <- tempfile(fileext = ".duckdb")

  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir)
  DBI::dbExecute(con, glue::glue("SET temp_directory = '{tempdir()}'"))

  con
}
