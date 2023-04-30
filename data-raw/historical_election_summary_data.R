

# ----- packages -----

library(tidyverse)
library(glue)
library(lubridate)

# ----- historical election summary data -----

# All congress/senate elections
type_elec <-
  dates_elections_spain |>
  filter(type_elec %in% c("congress", "senate")) |> pull(type_elec)
year <-
  dates_elections_spain |>
  filter(type_elec %in% c("congress", "senate")) |> pull(year)
month <-
  dates_elections_spain |>
  filter(type_elec %in% c("congress", "senate"))|> pull(month)

# Historical election data (by elections and date)
historical_summary_elections <-
  get_poll_station_data(type_elec, year, month) |>
  summarise(census_counting = sum(census_counting),
            valid_ballots = sum(valid_ballots),
            invalid_ballots = sum(invalid_ballots),
            total_ballots = valid_ballots + invalid_ballots,
            porc_valid =
              round(100 * valid_ballots / total_ballots, 3),
            porc_invalid =
              round(100 * invalid_ballots / total_ballots, 3),
            blank_ballots = sum(blank_ballots),
            party_ballots = sum(party_ballots),
            turnout = round(100 * total_ballots/census_counting, 3),
            .by = c("date_elec", "type_elec"))

# ----- use data -----
usethis::use_data(historical_summary_elections, overwrite = TRUE,
                  compress = "xz")

# ----- write_csv -----
write_csv(historical_summary_elections,
          file = "./data/csv/summary_data/historical_summary_elections.csv")


# ----- delete -----
rm(historical_summary_elections)

