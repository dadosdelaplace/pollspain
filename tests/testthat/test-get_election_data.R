test_that("get_elections_data works", {
  random_dates <-
    sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29",
                         "1993-06-06", "1996-03-03", "2000-03-12",
                         "2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 2,
           replace = FALSE)
  expect_equal(get_election_data(type_elec = "congress", date = random_dates,
                                 verbose = FALSE) |>
                 filter(is.na(id_candidacies) | is.na(id_candidacies_nat)) |>
                 nrow(), 0)

    random_dates <-
    sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29",
                         "1993-06-06", "1996-03-03", "2000-03-12",
                         "2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 2,
           replace = FALSE)
  expect_equal(get_election_data(type_elec = "congress", date = random_dates,
                                 verbose = FALSE) |>
                 summarise("party_ballots" = unique(party_ballots),
                           "sum_ballots" = sum(ballots),
                           .by = c("id_INE_poll_station", "id_elec")) |>
                 filter(sum_ballots != party_ballots) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29",
                         "1993-06-06", "1996-03-03", "2000-03-12",
                         "2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 2,
           replace = FALSE)
  expect_equal(get_election_data(type_elec = "congress", date = random_dates,
                                 verbose = FALSE) |>
                 distinct(id_INE_poll_station, id_elec, .keep_all = TRUE) |>
                 mutate("valid_compute" = party_ballots + blank_ballots,
                        "total_compute" = valid_compute + invalid_ballots) |>
                 filter(valid_compute != valid_ballots | total_compute != total_ballots) |>
                 nrow(), 0)

  random_dates <-
    sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29",
                         "1993-06-06", "1996-03-03", "2000-03-12",
                         "2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 2,
           replace = FALSE)
  expect_equal(get_election_data(type_elec = "congress", date = random_dates,
                                 verbose = FALSE) |>
                 distinct(id_INE_poll_station, id_elec, .keep_all = TRUE) |>
                 summarise("party_ballots" = sum(party_ballots),
                           "blank_ballots" = sum(blank_ballots),
                           "valid_ballots" = sum(valid_ballots),
                           "invalid_ballots" = sum(invalid_ballots),
                           "total_ballots" = sum(total_ballots)) |>
                 mutate("valid_compute" = party_ballots + blank_ballots,
                        "total_compute" = valid_compute + invalid_ballots) |>
                 filter(valid_compute != valid_ballots | total_compute != total_ballots) |>
                 nrow(), 0)
  expect_error(get_election_data(type_elec = "national", year = 2023, verbose = FALSE))
  expect_error(get_election_data(type_elec = "congress", date = "26-06-2016", verbose = FALSE))
  expect_error(get_election_data(type_elec = "congress", year = 2023, short_version = "yes", verbose = FALSE))
})

