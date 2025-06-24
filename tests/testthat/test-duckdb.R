test_that("duckdb works with temporary = FALSE and TRUE", {

  # Creating dir
  temp_db_dir <- file.path(tempdir(), "duckdb_test_scratch")
  dir.create(temp_db_dir, showWarnings = FALSE)

  # Creating connection
  db_path <- tempfile(tmpdir = temp_db_dir, fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Set temp_directory in an expliticy way
  DBI::dbExecute(con, paste0("SET temp_directory = '", temp_db_dir, "'"))

  # Dummy adta
  df <- data.frame(id = 1:3, nombre = c("a", "b", "c"))

  ## Test 1: copy_to with temporary = FALSE
  dplyr::copy_to(con, df, name = "table_persist", temporary = FALSE, overwrite = TRUE)
  res1 <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM table_persist")
  expect_equal(res1$n, 3)

  ## Test 2: copy_to with temporary = TRUE
  dplyr::copy_to(con, df, name = "table_temp", temporary = TRUE)
  res2 <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM table_temp")
  expect_equal(res2$n, 3)
})
