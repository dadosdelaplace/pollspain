test_that("summary_elections_data works", {

  # no missing candidacies
  random_dates <-
    sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29", "1993-06-06",
                         "1996-03-03", "2000-03-12", "2004-03-14", "2008-03-09",
                         "2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28",
                         "2019-11-10", "2023-07-24")), size = 3,
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

  # differences vs CEB: prov level
  random_dates <-
    sample(x = as_date(c("2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28",
                         "2019-11-10", "2023-07-24")), size = 2,
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
                 filter(if_all(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  # differences vs CEB: ccaa level
  random_dates <-
    sample(x = as_date(c("2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28",
                         "2019-11-10", "2023-07-24")), size = 2,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "ccaa",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 left_join(CEB_results |>
                             summarise(across(all_of(contains("ballots")), sum),
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
                 filter(if_all(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  # differences vs CEB: nat level
  random_dates <-
    sample(x = as_date(c("2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28",
                         "2019-11-10", "2023-07-24")), size = 2,
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
                 filter(if_all(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  # differences vs CEB: mun level (and the aggregate by prov)
  random_dates <-
    sample(x = as_date(c("2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28",
                         "2019-11-10", "2023-07-24")), size = 1,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "mun",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 mutate("id_INE_prov" =
                          str_extract(id_INE_mun, "[0-9]{2}-[0-9]{2}")) |>
                 summarise(across(all_of(contains("ballots")), sum),
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
                 filter(if_all(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  # differences vs CEB: mun district level (and the aggregate by prov)
  random_dates <-
    sample(x = as_date(c("2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28",
                         "2019-11-10", "2023-07-24")), size = 1,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "mun_district",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 mutate("id_INE_prov" =
                          str_extract(id_INE_mun_district, "[0-9]{2}-[0-9]{2}")) |>
                 summarise(across(all_of(contains("ballots")), sum),
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
                 filter(if_all(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  # differences vs CEB: sec level (and the aggregate by prov)
  random_dates <-
    sample(x = as_date(c("2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28",
                         "2019-11-10", "2023-07-24")), size = 1,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "sec",
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 mutate("id_INE_prov" =
                          str_extract(id_INE_sec, "[0-9]{2}-[0-9]{2}")) |>
                 summarise(across(all_of(contains("ballots")), sum),
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
                 filter(if_all(contains("ballots"),
                               function(x) { x > 1.01 | x < 0.99})) |>
                 nrow(), 0)

  # Correct seats

  random_dates <- "2015-12-20"
  random_dates <-
    sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29", "1993-06-06",
                         "1996-03-03", "2000-03-12", "2004-03-14", "2008-03-09",
                         "2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28",
                         "2019-11-10", "2023-07-24")), size = 1,
           replace = FALSE)
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = TRUE,
                                     level = "prov",
                                     method =
                                       sample(x = c("hondt", "hamilton", "vinton", "webster",
                                                           "sainte-lague", "hill", "huntington-hill",
                                                           "dean", "adams", "hagenbach", "fptp"), size = 1),
                                     verbose = FALSE) |>
                 summarise("total_seats" = sum(seats), .by = id_elec) |>
                 filter(total_seats != 350) |>  nrow(), 0)

  # Correct seats allocation by party with D'Hondt method

  random_dates <-
    sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29", "1993-06-06",
                         "1996-03-03", "2000-03-12", "2004-03-14", "2008-03-09",
                         "2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28",
                         "2019-11-10", "2023-07-24")), size = 2,
           replace = FALSE)
  seats <- seats_by_province |>
    filter(id_elec == paste0("02-", random_dates))

  expect_equal(summary_election_data(
    type_elec = "congress",
    date      = random_dates,
    by_parties= TRUE,
    level     = "prov",
    method    = "dhondt",
    verbose   = FALSE) |>
    right_join(seats, by = c("id_elec", "id_INE_prov", "id_candidacies_nat")) |>
    mutate(seats_difference = seats - real_seats) |>
    filter(seats_difference != 0) |>
    nrow(), 0)

  # is tibble?
  expect_equal(summary_election_data(type_elec = "congress",
                                     date = random_dates,
                                     by_parties = FALSE,
                                     level = "prov",
                                     verbose = FALSE) |>
                 is_tibble(), TRUE)

  expect_error(summary_election_data(type_elec = "congress",
                                     date = "2023", verbose = FALSE))
  expect_error(summary_election_data(type_elec = "congress",
                                     date = 2022, verbose = FALSE))
})



