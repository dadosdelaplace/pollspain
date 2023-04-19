# ----- packages -----

library(tidyverse)
library(glue)
library(lubridate)

# ----- get survey links from wikipedia -----

historical_survey_links_wikipedia <-
  tibble(date_elec =
           dates_elections_spain |>
           filter(cod_elec == "02" & year(date) >= 1982) |>
           pull(date),
         year = year(date_elec),
         type_elec = "congress",
         type_survey = "national",
         links = c(unique(glue("https://en.wikipedia.org/wiki/Opinion_polling_for_the_{year}_Spanish_general_election")),
                   "https://en.wikipedia.org/wiki/Opinion_polling_for_the_November_2019_Spanish_general_election")) |>
  add_row(date_elec = NA, year = 2023, type_elec = "congress", type_survey = "national",
          links = "https://en.wikipedia.org/wiki/Nationwide_opinion_polling_for_the_next_Spanish_general_election_(2019–2021)")

# ----- historical raw surveys -----

historical_raw_surveys <-
  get_raw_surveys_wiki(from = 1982, to = year(today()),
                       type_elec = "congress", type_survey = "national") |>
  mutate(type_elec = "congress", type_survey = "national") |>
  relocate(type_elec, type_survey, .before = everything())

# Historical surveys (preprocesses)
# historical_surveys <- preproc_raw_surveys(historical_raw_surveys)
historical_surveys <-
  get_surveys(exclude_NA_size = FALSE, exclude_exit_poll = FALSE,
              exclude_parties_poll = FALSE)

# ----- use data -----
usethis::use_data(historical_survey_links_wikipedia, overwrite = TRUE,
                  compress = "xz")
usethis::use_data(historical_raw_surveys, overwrite = TRUE,
                  compress = "xz")
usethis::use_data(historical_surveys, overwrite = TRUE,
                  compress = "xz")

