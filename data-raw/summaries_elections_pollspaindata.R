# ----- packages -----

library(tidyverse)
library(purrr)
library(lubridate)
library(pollspain)

# ----- loop dates -----

dates <-
  as_date(c("1982-10-28", "1986-06-22", "1989-10-29", "1993-06-06",
            "1996-03-03", "2000-03-12", "2004-03-14", "2008-03-09",
            "2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28",
            "2019-11-10", "2023-07-24"))

for (i in 1:length(dates)) {

  summary_tb <-
    summary_election_data(type_elec = "congress",
                          date = dates[i], by_parties = TRUE,
                          level = "all",
                          short_version = FALSE,
                          verbose = FALSE) |>
    mutate(across(where(is.character), \(x) enc2utf8(x)))

  if (nrow(summary_tb) <= 20) {
    stop(red("Ups! An error should be fixed"))
  }

  file_name <- paste0("summary_elec_all_", dates[i], ".rda")
  save(summary_tb,
       file = file.path("../pollspaindata/inst/extdata/summary_elec/", file_name),
       compress = "xz")

  summary_tb <-
    summary_election_data(type_elec = "congress",
                          date = dates[i], by_parties = TRUE,
                          level = "ccaa",
                          short_version = FALSE,
                          verbose = FALSE) |>
    mutate(across(where(is.character), \(x) enc2utf8(x)))

  if (nrow(summary_tb) <= 19*7) {
    stop(red("Ups! An error should be fixed"))
  }

  file_name <- paste0("summary_elec_ccaa_", dates[i], ".rda")
  save(summary_tb,
       file = file.path("../pollspaindata/inst/extdata/summary_elec/", file_name),
       compress = "xz")

  summary_tb <-
    summary_election_data(type_elec = "congress",
                          date = dates[i], by_parties = TRUE,
                          level = "prov",
                          short_version = FALSE,
                          verbose = FALSE) |>
    mutate(across(where(is.character), \(x) enc2utf8(x)))

  if (nrow(summary_tb) <= 52*7) {
    stop(red("Ups! An error should be fixed"))
  }

  file_name <- paste0("summary_elec_prov_", dates[i], ".rda")
  save(summary_tb,
       file = file.path("../pollspaindata/inst/extdata/summary_elec/", file_name),
       compress = "xz")
}
