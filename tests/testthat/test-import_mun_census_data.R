test_that("import mun census data", {
  random_dates <-
    sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29",
                         "1993-06-06", "1996-03-03", "2000-03-12",
                         "2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 3,
           replace = FALSE)
  expect_equal(import_mun_census_data(type_elec = "congress", date = random_dates,
                                      verbose = FALSE) |>
                 filter(is.na(id_INE_mun) | is.na(cod_INE_ccaa) |
                          is.na(ccaa) | is.na(cod_INE_prov) | is.na(prov) |
                          is.na(cod_INE_mun) | is.na(mun)) |>
                 nrow(), 0)

  expect_equal(import_mun_census_data(type_elec = "congress",
                                       date = "2019-04-28",
                                      verbose = FALSE) |> nrow(), 8130)

  random_dates <-
    sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29",
                         "1993-06-06", "1996-03-03", "2000-03-12",
                         "2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 3,
           replace = FALSE)
  expect_equal(import_mun_census_data(type_elec = "congress",
                                      date = random_dates,
                                      verbose = FALSE) |>
                 filter(is.na(id_INE_mun) | is.na(cod_INE_ccaa) | is.na(ccaa) |
                          is.na(cod_INE_prov) | is.na(prov) |
                          is.na(cod_INE_mun) | is.na(mun)) |>
                 nrow(), 0)
  expect_error(import_mun_census_data("national", 2023, verbose = FALSE))
  expect_error(import_mun_census_data("congress", verbose = FALSE))
  expect_error(import_mun_census_data("congress", year = 2023, verbose = "yes"))
  expect_error(import_mun_census_data("congress", year = 2018, verbose = FALSE))
  expect_error(import_mun_census_data("congress", date = "2019-05-01", verbose = FALSE))
  expect_error(import_mun_census_data("congress", year = 2018, verbose = FALSE))
})
