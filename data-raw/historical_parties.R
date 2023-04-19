
# ----- packages -----

library(tidyverse)
library(glue)
library(lubridate)

# ----- get unique parties -----

# Obtain unique parties for all elections
election_data <-
  get_elections_data(type_elec = rep("congress", 13),
                     year = c(1982, 1986, 1989, 1993, 1996, 2000,
                              2004, 2008, 2011, 2015, 2016, 2019, 2019),
                     month = c(10, 6, 10, 6, 3, 3, 3, 3, 11, 12, 6, 4, 11),
                     include_candidacies = TRUE,
                     include_candidates = FALSE) |>
  aggregate_election_data(level = "prov", by_parties = TRUE)

# Select just 3 columns
historical_parties <-
  election_data |>
  select(type_elec, abbrev_candidacies, name_candidacies)

# Recode parties
historical_parties <-
  historical_parties |>
  # Include parties for the surveys
  add_row(type_elec = "congress", abbrev_candidacies = "PAD",
          name_candidacies = "PARTIDO DE ACCION DEMOCRATICA") |>
  add_row(type_elec = "congress", abbrev_candidacies = "PDP",
          name_candidacies = "PARTIDO DEMOCRATA POPULAR") |>
  add_row(type_elec = "congress", abbrev_candidacies = "PDECAT",
          name_candidacies = "PARTIT DEMOCRATA EUROPEU CATALA") |>
  distinct(abbrev_candidacies, .keep_all = TRUE) |>
  drop_na(abbrev_candidacies) |>
  arrange(abbrev_candidacies)

# ----- use data -----
usethis::use_data(historical_parties, overwrite = TRUE,
                  compress = "xz")

