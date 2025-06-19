test_that("summary elections data", {
  setup({
    con <<- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  })

  teardown({
    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 2,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = TRUE,
                                     level = "prov",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 filter(is.na(id_candidacies) | is.na(abbrev_candidacies) |
                          is.na(name_candidacies)) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 2,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "prov",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 left_join(CEB_results,
                           by = c("id_elec", "id_INE_prov"),
                           suffix = c(".pollspain", ".CEB")) |>
                 summarise("ratio_total_ballots" =
                             total_ballots.pollspain / total_ballots.CEB,
                           "ratio_valid_ballots" =
                             valid_ballots.pollspain / valid_ballots.CEB,
                           "ratio_party_ballots" =
                             party_ballots.pollspain / party_ballots.CEB,
                           "ratio_blank_ballots" =
                             blank_ballots.pollspain / blank_ballots.CEB,
                           "ratio_invalid_ballots" =
                             invalid_ballots.pollspain / invalid_ballots.CEB,
                           .by = c(id_elec, id_INE_prov, prov.pollspain)) |>
                 filter(across(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 1,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "prov",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 left_join(CEB_results,
                           by = c("id_elec", "id_INE_prov"),
                           suffix = c(".pollspain", ".CEB")) |>
                 summarise("ratio_total_ballots" =
                             total_ballots.pollspain / total_ballots.CEB,
                           "ratio_valid_ballots" =
                             valid_ballots.pollspain / valid_ballots.CEB,
                           "ratio_party_ballots" =
                             party_ballots.pollspain / party_ballots.CEB,
                           "ratio_blank_ballots" =
                             blank_ballots.pollspain / blank_ballots.CEB,
                           "ratio_invalid_ballots" =
                             invalid_ballots.pollspain / invalid_ballots.CEB,
                           .by = c(id_elec, id_INE_prov, prov.pollspain)) |>
                 filter(across(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 1,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "ccaa",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 left_join(CEB_results |>
                             summarise(across(contains("ballots"), sum),
                                       .by = c(id_elec, cod_INE_ccaa, ccaa)),
                           by = c("id_elec", "id_INE_ccaa" = "cod_INE_ccaa"),
                           suffix = c(".pollspain", ".CEB")) |>
                 summarise("ratio_total_ballots" =
                             total_ballots.pollspain / total_ballots.CEB,
                           "ratio_valid_ballots" =
                             valid_ballots.pollspain / valid_ballots.CEB,
                           "ratio_party_ballots" =
                             party_ballots.pollspain / party_ballots.CEB,
                           "ratio_blank_ballots" =
                             blank_ballots.pollspain / blank_ballots.CEB,
                           "ratio_invalid_ballots" =
                             invalid_ballots.pollspain / invalid_ballots.CEB,
                           .by = c(id_elec, id_INE_ccaa, ccaa.pollspain)) |>
                 filter(across(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 1,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "all",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 left_join(CEB_results |>
                             summarise(across(contains("ballots"), sum),
                                       .by = c(id_elec)),
                           by = c("id_elec"),
                           suffix = c(".pollspain", ".CEB")) |>
                 summarise("ratio_total_ballots" =
                             total_ballots.pollspain / total_ballots.CEB,
                           "ratio_valid_ballots" =
                             valid_ballots.pollspain / valid_ballots.CEB,
                           "ratio_party_ballots" =
                             party_ballots.pollspain / party_ballots.CEB,
                           "ratio_blank_ballots" =
                             blank_ballots.pollspain / blank_ballots.CEB,
                           "ratio_invalid_ballots" =
                             invalid_ballots.pollspain / invalid_ballots.CEB,
                           .by = c(id_elec)) |>
                 filter(across(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 1,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "mun",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 mutate("id_INE_prov" =
                          str_extract(id_INE_mun, "[0-9]{2}-[0-9]{2}")) |>
                 summarise(across(contains("ballots"), sum),
                           .by = c(id_elec, id_INE_prov, prov)) |>
                 left_join(CEB_results,
                           by = c("id_elec", "id_INE_prov"),
                           suffix = c(".pollspain", ".CEB")) |>
                 summarise("ratio_total_ballots" =
                             total_ballots.pollspain / total_ballots.CEB,
                           "ratio_valid_ballots" =
                             valid_ballots.pollspain / valid_ballots.CEB,
                           "ratio_party_ballots" =
                             party_ballots.pollspain / party_ballots.CEB,
                           "ratio_blank_ballots" =
                             blank_ballots.pollspain / blank_ballots.CEB,
                           "ratio_invalid_ballots" =
                             invalid_ballots.pollspain / invalid_ballots.CEB,
                           .by = c(id_elec, id_INE_prov, prov.pollspain)) |>
                 filter(across(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 1,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "mun_district",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 mutate("id_INE_prov" =
                          str_extract(id_INE_mun_district, "[0-9]{2}-[0-9]{2}")) |>
                 summarise(across(contains("ballots"), sum),
                           .by = c(id_elec, id_INE_prov, prov)) |>
                 left_join(CEB_results,
                           by = c("id_elec", "id_INE_prov"),
                           suffix = c(".pollspain", ".CEB")) |>
                 summarise("ratio_total_ballots" =
                             total_ballots.pollspain / total_ballots.CEB,
                           "ratio_valid_ballots" =
                             valid_ballots.pollspain / valid_ballots.CEB,
                           "ratio_party_ballots" =
                             party_ballots.pollspain / party_ballots.CEB,
                           "ratio_blank_ballots" =
                             blank_ballots.pollspain / blank_ballots.CEB,
                           "ratio_invalid_ballots" =
                             invalid_ballots.pollspain / invalid_ballots.CEB,
                           .by = c(id_elec, id_INE_prov, prov.pollspain)) |>
                 filter(across(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 1,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "sec",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 mutate("id_INE_prov" =
                          str_extract(id_INE_sec, "[0-9]{2}-[0-9]{2}")) |>
                 summarise(across(contains("ballots"), sum),
                           .by = c(id_elec, id_INE_prov, prov)) |>
                 left_join(CEB_results,
                           by = c("id_elec", "id_INE_prov"),
                           suffix = c(".pollspain", ".CEB")) |>
                 summarise("ratio_total_ballots" =
                             total_ballots.pollspain / total_ballots.CEB,
                           "ratio_valid_ballots" =
                             valid_ballots.pollspain / valid_ballots.CEB,
                           "ratio_party_ballots" =
                             party_ballots.pollspain / party_ballots.CEB,
                           "ratio_blank_ballots" =
                             blank_ballots.pollspain / blank_ballots.CEB,
                           "ratio_invalid_ballots" =
                             invalid_ballots.pollspain / invalid_ballots.CEB,
                           .by = c(id_elec, id_INE_prov, prov.pollspain)) |>
                 filter(across(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "prov",
                                     verbose = FALSE) |>
                 is.tibble(), TRUE)

  expect_error(summary_election_data(type_elec = "congress",
                                     date = "2023", verbose = FALSE))
  expect_error(summary_election_data(type_elec = "congress",
                                     date = 2022, verbose = FALSE))
})



