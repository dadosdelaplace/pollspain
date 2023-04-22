
# ----- packages -----

library(tidyverse)
library(glue)
library(lubridate)

# ----- import poll station data (prefix 09) from MIR -----

# year, month: year and month of election to be unloaded.
# base_url: a common path from which data will be downloaded
# encoding: encoding format to be passed to read_lines() function
# starts,ends: cut-off points for splitting raw .DAT files into a
# dataset with several variables.

import_poll_stations_MIR_files <-
  function(type_elec, year, month,
           base_url =
             "https://infoelectoral.interior.gob.es/estaticos/docxl/apliextr/",
           encoding = "Latin1",
           starts = c(1, 3, 7, 9, 10, 12, 14, 17, 19, 23, 24,
                      31, 38, 45, 52, 59, 66, 73, 80),
           ends = c(2, 6, 8, 9, 11, 13, 16, 18, 22, 23, 30,
                    37, 44, 51, 58, 65, 72, 79, 86)) {

    # Code of election
    cod_elec <- type_to_code_election(type_elec = type_elec)

    # Check: if elections required are allowed
    char_month <- str_pad(month, pad = "0", width = 2)
    join_result <-
      dates_elections_spain |>
      inner_join(tibble(cod_elec, type_elec, year, month),
                 by = c("cod_elec", "type_elec", "year", "month")) |>
      nrow()
    if (join_result == 0) {

      stop(glue("No {type_elec} elections are available in {char_month}-{year}"))

    }

    # Check: if base_url and encoding are valid
    if (!is.character(base_url) | !is.character(encoding)) {

      stop("Parameters 'base_url' and 'encoding' must be character")

    }

    # Check: if lengths of starts and ends are equal
    if (length(starts) != length(ends)) {

      stop("Length of vectors 'starts' and 'ends' must be equal")

    }

    # Build the url (.zip file)
    url <- glue("{base_url}{cod_elec}{year}{char_month}_MESA.zip")

    # Build temporal directory
    temp <- tempfile(tmpdir = tempdir(), fileext = ".zip")
    download.file(url, temp, mode = "wb")
    unzip(temp, overwrite = TRUE, exdir = tempdir())

    # Import raw data
    path <-
      glue("{tempdir()}/09{cod_elec}{str_sub(year, start = 3, end = 4)}{char_month}.DAT")
    raw_file <-
      as_tibble(read_lines(file = path,
                           locale = locale(encoding = encoding)))

    # Delete temporary dir
    try(file.remove(list.files(tempdir(), full.names = TRUE, recursive = TRUE)),
        silent = TRUE)

    # Check if there exists file
    if (nrow(raw_file) == 0) {

      return(raw_file)

    } else {
      # Process variables following the instructions of register
      poll_stations <-
        raw_file |>
        mutate(cod_elec = str_sub(value, start = starts[1], end = ends[1]),
               type_elec = type_elec,
               year =
                 as.numeric(str_sub(value, starts[2], end = ends[2])),
               month =
                 as.numeric(str_sub(value, start = starts[3], end = ends[3])),
               turn =
                 as.numeric(str_sub(value, start = starts[4], end = ends[4])),
               cod_MIR_ccaa =
                 str_trim(str_sub(value, start = starts[5], end = ends[5])),
               cod_INE_prov =
                 str_trim(str_sub(value, start = starts[6], end = ends[6])),
               cod_INE_mun =
                 str_trim(str_sub(value, start = starts[7], end = ends[7])),
               id_MIR_mun = glue("{cod_MIR_ccaa}-{cod_INE_prov}-{cod_INE_mun}"),
               cod_mun_district =
                 str_trim(str_sub(value, start = starts[8], end = ends[8])),
               cod_sec =
                 str_trim(str_sub(value, start = starts[9], end = ends[9])),
               cod_poll_station =
                 str_trim(str_sub(value, start = starts[10], end = ends[10])),
               # pop_res who are allowed to vote
               census_INE =
                 as.numeric(str_sub(value, start = starts[11], end = ends[11])),
               # census_ine after claims (or C.E.R.A, Spanish citizens absent)
               census_counting =
                 as.numeric(str_sub(value, start = starts[12], end = ends[12])),
               # C.E.R.E (census of foreign citizens)
               census_cere =
                 as.numeric(str_sub(value, start = starts[13], end = ends[13])),
               voters_cere =
                 as.numeric(str_sub(value, start = starts[14], end = ends[14])),
               ballots_1 =
                 as.numeric(str_sub(value, start = starts[15], end = ends[15])),
               ballots_2 =
                 as.numeric(str_sub(value, start = starts[16], end = ends[16])),
               blank_ballots =
                 as.numeric(str_sub(value, start = starts[17], end = ends[17])),
               invalid_ballots =
                 as.numeric(str_sub(value, start = starts[18], end = ends[18])),
               party_ballots =
                 as.numeric(str_sub(value, start = starts[19],
                                    end = ends[19]))) |>
        select(-value) |>
        select(where(function(x) {
          !all(is.na(x))
        }))

      # Join with dates of elections
      poll_stations <-
        poll_stations |>
        left_join(dates_elections_spain |> select(-topic),
                  by = c("cod_elec", "type_elec", "year", "month")) |>
        select(-year, -month, -day)

      # Rename and relocate
      poll_stations <-
        poll_stations |>
        rename(date_elec = date) |>
        relocate(date_elec, .after = type_elec) |>
        relocate(id_MIR_mun, .after = date_elec) |>
        relocate(turn, .after = cod_poll_station)

      # C.E.R.A (census of Spanish citizens absent from Spain)
      # are asigned cod_INE_mun = "999", cod_mun_district = "09"
      # cod_sec = "0000", cod_poll_station = "U"
      # For each ccaa is summarized with cod_INE_prov = "99"
      # and they should be removed
      poll_stations <-
        poll_stations |>
        filter(!(cod_INE_prov == "99" & cod_INE_mun == "999"))

      # output
      return(poll_stations)
    }
  }

# ----- poll data congress -----
congress_elec <-
  dates_elections_spain |>
  filter(cod_elec == "02") |>
  drop_na(year, month) |>
  select(type_elec, year, month)

historical_poll_station_data_congress <-
  congress_elec |>
  rowwise() |>
  reframe(import_poll_stations_MIR_files(type_elec, year, month)) |>
  select(-any_of("value"))

# ----- poll data congress -----
senate_elec <-
  dates_elections_spain |>
  filter(cod_elec == "03") |>
  drop_na(year, month) |>
  select(type_elec, year, month)

historical_poll_station_data_senate <-
  senate_elec |>
  rowwise() |>
  reframe(import_poll_stations_MIR_files(type_elec, year, month)) |>
  select(-any_of("value"))

# ----- use data -----
historical_raw_poll_station_data <-
  bind_rows(historical_poll_station_data_congress,
            historical_poll_station_data_senate)

# too much size
# usethis::use_data(historical_raw_poll_station_data, overwrite = TRUE,
#                   compress = "xz")

# ----- write_csv -----
historical_raw_poll_station_data |>
  split(historical_raw_poll_station_data$type_elec) |>
  map(function(x) { split(x, x$date_elec) }) |>
  map(function(y) { map(y, function(x) {
    write_csv(x, file =
                glue("./data/csv/pollstation/raw_poll_station_data_{unique(x$type_elec)}_{year(unique(x$date_elec))}_{month(unique(x$date_elec))}.csv"))})})

# ----- delete -----
rm(list = c("historical_poll_station_data_congress",
            "historical_poll_station_data_senate",
            "historical_raw_poll_station_data"))




