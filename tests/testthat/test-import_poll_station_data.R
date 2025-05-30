test_that("import poll station data", {
  expect_equal(import_poll_station_data(type_elec = "congress",
                                        date = sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29", "1993-06-06", "1996-03-03", "2000-03-12", "2004-03-14", "2008-03-09", "2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28", "2019-11-10", "2023-07-24")), size = 2),
                                        verbose = FALSE) |>
                 mutate("valid_compute" = blank_ballots + party_ballots,
                        "total_compute" = blank_ballots + invalid_ballots + party_ballots) |>
                 filter(total_compute != total_ballots | valid_compute != valid_ballots) |>
                 nrow(), 0)
  expect_equal(all(import_poll_station_data(type_elec = "congress", date = sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29", "1993-06-06", "1996-03-03", "2000-03-12", "2004-03-14", "2008-03-09", "2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28", "2019-11-10", "2023-07-24")), size = 2),
                                            verbose = FALSE) |>
                   summarise("dup" = n_distinct( id_INE_poll_station) - n(), .by = "id_elec") |>
                   pull(dup) == 0), TRUE)
  expect_equal(import_poll_station_data(type_elec = "congress",
                                        date = sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29", "1993-06-06", "1996-03-03", "2000-03-12", "2004-03-14", "2008-03-09", "2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28", "2019-11-10", "2023-07-24")), size = 2),
                                        verbose = FALSE) |>
                 filter(str_detect(id_INE_poll_station, "-999-")) |>
                 summarise("CERA" = n() - 52, .by = "id_elec") |>
                 filter(CERA != 0) |> nrow(), 0)
  expect_equal(import_poll_station_data(type_elec = "congress",
                                        date = sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29", "1993-06-06", "1996-03-03", "2000-03-12", "2004-03-14", "2008-03-09", "2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28", "2019-11-10", "2023-07-24")), size = 2),
                                        verbose = FALSE) |>
                 summarise("blank_ballots" = sum(blank_ballots), "party_ballots" = sum(party_ballots), "invalid_ballots" = sum(invalid_ballots), "valid_ballots" = sum(valid_ballots), "total_ballots" = sum(total_ballots), .by = "id_elec") |>
                 mutate("valid_compute" = blank_ballots + party_ballots,
                        "total_compute" = blank_ballots + invalid_ballots + party_ballots) |>
                 filter(valid_compute != valid_ballots | total_compute != total_ballots) |>
                 nrow(), 0)
  expect_error(import_poll_station_data(type_elec = "national", year = 2019))
  expect_error(import_poll_station_data(type_elec = "congress", year = 2018))
  expect_error(import_poll_station_data(type_elec = "congress", year = 2023, prec_round = -1))
  expect_error(import_poll_station_data(type_elec = "congress", year = 2019,
                                        short_version = "yes"))
})

