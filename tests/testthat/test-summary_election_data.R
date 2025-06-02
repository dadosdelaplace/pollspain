test_that("summary elections data", {
  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 2)

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
                               function(x) { x > 1.005 | x < 0.995})) |>
                 nrow(), 0)

  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "prov",
                                     lazy_duckdb = TRUE,
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 collect() |>
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
                               function(x) { x > 1.005 | x < 0.995})) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 2)

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
                               function(x) { x > 1.005 | x < 0.995})) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 1)

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
                               function(x) { x > 1.005 | x < 0.995})) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 1)

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
                               function(x) { x > 1.005 | x < 0.995})) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 1)

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
                               function(x) { x > 1.005 | x < 0.995})) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 1)

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
                               function(x) { x > 1.005 | x < 0.995})) |>
                 nrow(), 0)

  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "prov",
                                     verbose = FALSE) |>
                 is.data.frame(), TRUE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     lazy_duckdb = TRUE,
                                     level = "prov",
                                     verbose = FALSE) |>
                 is.data.frame(), FALSE)

  expect_error(summary_election_data(type_elec = "congress",
                                     date = "2023"))
  expect_error(summary_election_data(type_elec = "congress",
                                     date = 2022))
})



