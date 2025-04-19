


# ----- packages -----

library(tidyverse)
library(glue)
library(lubridate)

# ----- historical election data -----

historical_election_data <-
  get_elections_data(type_elec = rep("congress", 13),
                     year = c(1982, 1986, 1989, 1993, 1996, 2000,
                              2004, 2008, 2011, 2015, 2016, 2019, 2019),
                     month = c(10, 6, 10, 6, 3, 3, 3, 3, 11, 12, 6, 4, 11),
                     include_candidacies = TRUE,
                     include_candidates = FALSE) |>
  aggregate_election_data(level = "all", by_parties = TRUE)

historical_prov_election_data <-
  get_elections_data(type_elec = rep("congress", 13),
                     year = c(1982, 1986, 1989, 1993, 1996, 2000,
                              2004, 2008, 2011, 2015, 2016, 2019, 2019),
                     month = c(10, 6, 10, 6, 3, 3, 3, 3, 11, 12, 6, 4, 11),
                     include_candidacies = TRUE,
                     include_candidates = FALSE) |>
  aggregate_election_data(level = "prov", by_parties = TRUE)

historical_ccaa_election_data <-
  get_elections_data(type_elec = rep("congress", 13),
                     year = c(1982, 1986, 1989, 1993, 1996, 2000,
                              2004, 2008, 2011, 2015, 2016, 2019, 2019),
                     month = c(10, 6, 10, 6, 3, 3, 3, 3, 11, 12, 6, 4, 11),
                     include_candidacies = TRUE,
                     include_candidates = FALSE) |>
  aggregate_election_data(level = "ccaa", by_parties = TRUE)

historical_ccaa_election_data <-
  get_elections_data(type_elec = rep("congress", 13),
                     year = c(1982, 1986, 1989, 1993, 1996, 2000,
                              2004, 2008, 2011, 2015, 2016, 2019, 2019),
                     month = c(10, 6, 10, 6, 3, 3, 3, 3, 11, 12, 6, 4, 11),
                     include_candidacies = TRUE,
                     include_candidates = FALSE) |>
  aggregate_election_data(level = "mun", by_parties = TRUE)

# ----- use data -----
usethis::use_data(historical_election_data, overwrite = TRUE,
                  compress = "xz")
usethis::use_data(historical_ccaa_election_data, overwrite = TRUE,
                  compress = "xz")
usethis::use_data(historical_prov_election_data, overwrite = TRUE,
                  compress = "xz")
usethis::use_data(historical_mun_election_data, overwrite = TRUE,
                  compress = "xz")

# ----- write_csv -----
write_csv(historical_election_data, "./data/historical_election_data.csv")
write_csv(historical_ccaa_election_data, "./data/historical_ccaa_election_data.csv")
write_csv(historical_prov_election_data, "./data/historical_prov_election_data.csv")
write_csv(historical_mun_election_data, "./data/historical_mun_election_data.csv")

