

# import files with prefix 03 from MIR

# ----- packages -----

library(tidyverse)
library(glue)
library(lubridate)

# ----- Candidacies congress -----
congress_elec <-
  dates_elections_spain |>
  filter(cod_elec == "02") |>
  drop_na(year, month) |>
  select(type_elec, year, month)

historical_raw_candidacies_congress <-
  congress_elec |>
  rowwise() |>
  reframe(import_raw_candidacies_file(type_elec, year, month))

# ----- Candidacies senate -----

senate_elec <-
  dates_elections_spain |>
  filter(cod_elec == "03") |>
  drop_na(year, month) |>
  select(type_elec, year, month)

historical_raw_candidacies_senate <-
  senate_elec |>
  rowwise() |>
  reframe(import_raw_candidacies_file(type_elec, year, month))

# ----- use data -----
usethis::use_data(historical_raw_candidacies_congress, overwrite = TRUE,
                  compress = "xz")
usethis::use_data(historical_raw_candidacies_senate, overwrite = TRUE,
                  compress = "xz")

# ----- write_csv -----
write_csv(historical_raw_candidacies_congress,
          "./data/historical_raw_candidacies_congress.csv")
write_csv(historical_raw_candidacies_senate,
          "./data/historical_raw_candidacies_senate.csv")


