
# ----- elecciones -----

# todas elecciones por mesa
all_elections <-
  get_elections_data(type_elec = rep("congress", 13),
                     year = c(1982, 1986, 1989, 1993, 1996, 2000,
                              2004, 2008, 2011, 2015, 2016, 2019, 2019),
                     month = c(10, 6, 10, 6, 3, 3, 3, 3, 11, 12, 6, 4, 11),
                     include_candidacies = FALSE,
                     include_candidates = FALSE) |>
  aggregate_election_data(level = "all", by_parties = FALSE)

# todas elecciones por municipio con votos partidos
mun_election <-
  get_elections_data(type_elec = rep("congress", 13),
                     year = c(1982, 1986, 1989, 1993, 1996, 2000,
                              2004, 2008, 2011, 2015, 2016, 2019, 2019),
                     month = c(10, 6, 10, 6, 3, 3, 3, 3, 11, 12, 6, 4, 11),
                     include_candidacies = TRUE,
                     include_candidates = FALSE) |>
  aggregate_election_data(level = "mun", by_parties = TRUE)

# todas elecciones por provincias con votos
historical_prov_election_data <-
  get_elections_data(type_elec = rep("congress", 13),
                     year = c(1982, 1986, 1989, 1993, 1996, 2000,
                              2004, 2008, 2011, 2015, 2016, 2019, 2019),
                     month = c(10, 6, 10, 6, 3, 3, 3, 3, 11, 12, 6, 4, 11),
                     include_candidacies = TRUE,
                     include_candidates = FALSE) |>
  aggregate_election_data(level = "prov", by_parties = TRUE)
usethis::use_data(historical_prov_election_data, overwrite = TRUE,
                  compress = "xz")




# ----- encuestas -----

# todas
all_surveys <- get_surveys()

min_size_surveys <-
  get_surveys(min_size = 1000, min_days_to_elec = 15, max_days_to_elec = 90)
