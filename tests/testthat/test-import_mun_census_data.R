test_that("import mun census data", {
  expect_equal(import_mun_census_data(type_elec = "congress", year = 2019,
                                      verbose = FALSE) |>
                 filter(is.na(id_INE_mun) | is.na(cod_INE_ccaa) |
                          is.na(ccaa) | is.na(cod_INE_prov) | is.na(prov) |
                          is.na(cod_INE_mun) | is.na(mun)) |>
                 nrow(), 0)
  expect_equal(import_mun_census_data(type_elec = "congress",
                                       date = "2019-04-28",
                                      verbose = FALSE) |> nrow(), 8130)
  expect_equal(import_mun_census_data(type_elec = "congress",
                                      year = c(2019, 2016, 1982, 1986),
                                      verbose = FALSE) |>
                 filter(is.na(id_INE_mun) | is.na(cod_INE_ccaa) | is.na(ccaa) |
                          is.na(cod_INE_prov) | is.na(prov) |
                          is.na(cod_INE_mun) | is.na(mun)) |>
                 nrow(), 0)
  expect_equal(import_mun_census_data(type_elec = "congress",
                                      year = c(1989, 1993, 2023),
                                      verbose = FALSE) |>
                 nrow(), 24274)
  expect_error(import_mun_census_data("national", 2019, verbose = FALSE))
  expect_error(import_mun_census_data("congress", verbose = FALSE))
  expect_error(import_mun_census_data("congress", year = 2023, verbose = "yes"))
  expect_error(import_mun_census_data("congress", year = 2018, verbose = FALSE))
  expect_error(import_mun_census_data("congress", date = "2019-05-01", verbose = FALSE))
  expect_error(import_mun_census_data("congress", year = 2018, verbose = FALSE))
})
