
#' @title Conversion type election to code
#'
#' @description Conversion the type of election (referendum,
#' congress, etc) to a properly code (according to documentation)
#' available in Spanish Ministry of Interior
#'
#' @param type_elec type elections for which data is available.
#' It should be one of the following values: "referendum",
#' "congress", "senate", "local", "cabildo" (Canarian council)
#' or "EU".
#'
#' @return A vector of string codes (\code{cod_elec}) as follows:
#' \itemize{
#'   \item \code{"01"}: referendum.
#'   \item \code{"02"}: congress.
#'   \item \code{"03"}: senate.
#'   \item \code{"04"}: local elections.
#'   \item \code{"06"}: cabildo - Canarian council - elections).
#'   \item \code{"07"}: European Parlament elections.
#' }
#'
#' @author Javier Álvarez-Liébana.
#' @source Data extracted from
#' \url{https://infoelectoral.interior.gob.es/opencms/es/elecciones-celebradas/area-de-descargas/}{Spanish Ministry of Interior}
#' @keywords utils
#' @name type_to_code_election
#'
#' @examples
#' ## Convert type to code
#'
#' # Right examples
#' type_to_code_election(type_elec = "congress")
#' type_to_code_election(type_elec = "senate")
#' type_to_code_election(type_elec = "local")
#'
#' \dontrun{
#' # Wrong examples
#' type_to_code_election(type_elec = "hi!")
#' type_to_code_election(type_elec = "nop")
#' type_to_code_election(type_elec = "national")
#' }
#'
#' @export
type_to_code_election <- function(type_elec) {

  # Check: if value in type_elec is allowed
  if (!all(type_elec %in%
        c("referendum", "congress", "senate", "local", "regional",
          "cabildo", "EU"))) {

    stop("Input argument type_elec is not valid: it must be taken one of the following values: 'referendum', 'congress',  'senate', 'local', 'cabildo' or 'EU'")

  }

  # Convert type to code
  cod_elec <-
    ifelse(type_elec == "referendum", "01",
           ifelse(type_elec == "congress", "02",
                  ifelse(type_elec == "senate", "03",
                         ifelse(type_elec == "local", "04",
                                ifelse(type_elec == "regional", "05",
                                       ifelse(type_elec == "cabildo",
                                              "06", "07"))))))

  # output
  return(cod_elec)
}


#' @title Extract region codes from the poll stations codes
#'
#' @description Extract region codes (for aggregation levels ccaa, prov, mun,
#' municipal districts and census sections) for a given poll station code
#' provided by Spanish Ministry of Interior (MIR) and Spanish Statistical
#' Office (INE)
#'
#' @param id_INE_poll_station poll station code. It should be a string vector
#' with tokens between 18 and 19 characters with 5 '-' according to the INE/MIR
#' format").
#' @param level aggregation level, for which we want to extract codes. It
#' should be taken from the following values: 'ccaa', 'prov', 'mun',
#' 'mun-district', 'sec' or 'poll-station'
#' @param full_cod flag to indicate if codes should be provided in a full format
#' (including codes of more aggregated levels) or not. Defaults to
#' \code{FALSE}.
#'
#' @return A string code subtract from the whole code the properly id for the
#' aggregation level required.
#'
#' @author Javier Álvarez-Liébana.
#' @keywords utils
#' @name extract_code
#'
#' @examples
#'
#' ## Extracting codes
#'
#' # Code for Adra ("003"), from province of Almeria ("04") and ccaa of
#' # Andalucia ("01"), first municipal district ("01"), census sections district
#' # ("004") and poll station "B"
#' id_INE_poll_station <- "01-04-003-01-004-B"
#'
#' # Right examples
#' extract_code(id_INE_poll_station, level = "mun", full_cod = FALSE)
#' extract_code(id_INE_poll_station, level = "mun", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "prov", full_cod = FALSE)
#' extract_code(id_INE_poll_station, level = "prov", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "ccaa", full_cod = FALSE)
#' extract_code(id_INE_poll_station, level = "ccaa", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "mun-district", full_cod = TRUE)
#'
#' \dontrun{
#' # Wrong examples
#' extract_code(id_INE_poll_station, level = "prov", full_cod = false)
#' extract_code(id_INE_poll_station, level = "province", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "muni", full_cod = "all")
#' extract_code(id_INE_poll_station, level = "poll", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "district", full_cod = TRUE)
#' }
#'
#' @export
extract_code <-
  function(id_INE_poll_station, level = "mun", full_cod = FALSE) {

    # Check: if id_INE_poll_station was provided in a valid format
    if (!(is.character(id_INE_poll_station) &
          all(between(nchar(id_INE_poll_station), 18, 19)) &
          length(str_extract_all(id_INE_poll_station, "-")[[1]]) == 5)) {

      stop("Argument 'id_INE_poll_station' must be a string of 18-19 characters with 5 '-' according to the INE format")
    }

    # Check: if level takes an allowed value
    if (!(level %in% c("ccaa", "prov", "mun", "mun-district",
                       "sec", "poll-station"))) {

      stop("Aggregation level provided by 'level' parameter must be taken from the following values: 'ccaa', 'prov', 'mun', 'mun-district', 'sec', 'poll-station'")

    }

    # Check: if full_codi is a logical variable
    if (!is.logical(full_cod)) {

      stop("Parameter 'full_cod' must be a logical variable, just TRUE/FALSE are avoided")

    }

    # Split id INE
    id_split <- str_split(id_INE_poll_station, "-")

    # Compute the number of elements
    i <- ifelse(level == "ccaa", 1,
             ifelse(level == "prov", 2,
                    ifelse(level == "mun", 3,
                           ifelse(level == "mun-district", 4,
                                  ifelse(level == "sec", 5, 6)))))

    # Access to elements of the list
    if (full_cod) {

      cod <- id_split |>
        map_chr(function(x) {
          str_c(x[1:i], collapse = "-")
          })

    } else {

      cod <-
        id_split |>
        map_chr(function(x) {
          x[i]
          })

    }

    # Output
    return(cod)
}


#' @title Import raw data files of elections
#'
#' @description Import raw data files of elections provided by Spanish Ministry
#' of Interior (MIR)
#'
#' @inheritParams type_to_code_election
#' @param year,month Year and month of election to be unloaded. Please be sure
#' (see \code{dates_elections_spain}) that elections of the provided type are
#' available for the given year and month.
#' @param base_url A common path from which data will be downloaded
#' @param encoding Encoding format to be passed to \code{read_lines()} function
#' @param starts,ends Cut-off points for splitting raw .DAT files into a
#' dataset with several variables, according to the documentation provided by
#' MIR files.
#'
#' @return A tibble with as many rows as poll stations and a set of the
#' following variables
#' \itemize{
#'   \item \code{"01"}: referendum.
#'   \item \code{"02"}: congress.
#'   \item \code{"03"}: senate.
#'   \item \code{"04"}: local elections.
#'   \item \code{"06"}: cabildo - Canarian council - elections).
#'   \item \code{"07"}: European Parlament elections.
#' }
#'
#' @author Javier Álvarez-Liébana.
#' @source Data extracted from
#' \url{https://infoelectoral.interior.gob.es/opencms/es/elecciones-celebradas/area-de-descargas/}
#' @keywords utils
#' @name import_raw_elections_files
#'
#' @examples
#' ## ...
#' @export
import_raw_candidacies_file <-
  function(type_elec, year, month,
           base_url =
             "https://infoelectoral.interior.gob.es/estaticos/docxl/apliextr/",
           encoding = "Latin1", starts = c(1, 3, 7, 9, 15, 65, 215, 221, 227),
           ends = c(2, 6, 8, 14, 64, 214, 220, 226, 232)) {

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

    # Import raw data (files 03 from MIR)
    path <-
      glue("{tempdir()}/03{cod_elec}{str_sub(year, start = 3, end = 4)}{char_month}.DAT")
    raw_file <-
      as_tibble(read_lines(file = path,
                           locale = locale(encoding = encoding)))

    # Delete temporary dir
    try(file.remove(list.files(tempdir(), full.names = TRUE, recursive = TRUE)),
        silent = TRUE)

    # Process variables following the instructions of register
    candidacies <-
      raw_file |>
      mutate(cod_elec = str_sub(value, start = starts[1], end = ends[1]),
             type_elec = type_elec,
             year = as.numeric(str_sub(value, starts[2], end = ends[2])),
             month =
               as.numeric(str_sub(value, start = starts[3], end = ends[3])),
             id_candidacies =
               str_trim(str_sub(value, start = starts[4], end = ends[4])),
             abbrev_candidacies =
               str_to_upper(str_trim(str_sub(value, start = starts[5],
                                             end = ends[5]))),
             name_candidacies =
               str_to_upper(str_trim(str_sub(value, start = starts[6],
                                             end = ends[6]))),
             cod_candidacies_prov =
               str_trim(str_sub(value, start = starts[7], end = ends[7])),
             cod_candidacies_ccaa =
               str_trim(str_sub(value, start = starts[8], end = ends[8])),
             cod_candidacies_nat =
               str_trim(str_sub(value, start = starts[9], end = ends[9]))) |>
      select(-value) |>
      select(where(function(x) {
        !all(is.na(x))
        }))

    # Join with dates of elections
    candidacies <-
      candidacies |>
      left_join(dates_elections_spain |> select(-topic),
                by = c("cod_elec", "type_elec", "year", "month"))  |>
      select(-year, -month, -day)

    # Rename and relocate
    candidacies <-
      candidacies |>
      rename(date_elec = date) |>
      relocate(date_elec, .after = type_elec)

    # output
    return(candidacies)
  }

#' @rdname import_raw_elections_files
#' @export
import_raw_candidates_file <-
  function(type_elec, year, month,
           base_url =
             "https://infoelectoral.interior.gob.es/estaticos/docxl/apliextr/",
           encoding = "Latin1",
           starts = c(1, 3, 7, 9, 10, 12, 13, 16, 22, 25, 26,
                      51, 76, 101, 102, 104, 106, 110, 120),
           ends = c(2, 6, 8, 9, 11, 12, 15, 21, 24, 25, 50,
                    75, 100, 101, 103, 105, 109, 119, 120)) {

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
      glue("{tempdir()}/04{cod_elec}{str_sub(year, start = 3, end = 4)}{char_month}.DAT")
    raw_file <-
      as_tibble(read_lines(file = path,
                           locale = locale(encoding = encoding)))

    # Delete temporary dir
    try(file.remove(list.files(tempdir(), full.names = TRUE, recursive = TRUE)),
        silent = TRUE)

    # Process variables following the instructions of register
    candidates <-
      raw_file |>
      mutate(cod_elec = str_sub(value, start = starts[1], end = ends[1]),
             type_elec = type_elec,
             year = as.numeric(str_sub(value, starts[2], end = ends[2])),
             month =
               as.numeric(str_sub(value, start = starts[3], end = ends[3])),
             turn =
               as.numeric(str_sub(value, start = starts[4], end = ends[4])),
             cod_INE_prov = str_sub(value, start = starts[5], end = ends[5]),
             cod_mun_district =
               str_sub(value, start = starts[6], end = ends[6]),
             cod_INE_mun = str_sub(value, start = starts[7], end = ends[7]),
             id_candidacies = str_sub(value, start = starts[8], end = ends[8]),
             order =
               as.numeric(str_sub(value, start = starts[9], end = ends[9])),
             holder =
               str_sub(value, start = starts[10], end = ends[10]) == "T",
             name =
               str_trim(str_sub(value, start = starts[11], end = ends[11])),
             surname1 =
               str_trim(str_sub(value, start = starts[12], end = ends[12])),
             surname2 =
               str_trim(str_sub(value, start = starts[13], end = ends[13])),
             surname = str_trim(glue("{surname1} {surname2}")),
             sex = str_sub(value, start = starts[14], end = ends[14]),
             birth_day = str_sub(value, start = starts[15], end = ends[15]),
             birth_month =
               as.numeric(str_sub(value, start = starts[16], end = ends[16])),
             birth_year =
               as.numeric(str_sub(value, start = starts[17], end = ends[17])),
             birthdate =
               suppressWarnings(as_date(
                 glue("{birth_year}-{birth_month}-{birth_day}"))),
             id_card =
               str_sub(value, start = starts[18], end = ends[18]),
             elected =
               str_sub(value, start = starts[19], end = ends[19]) == "S") |>
      select(-value, -surname1, -surname2, -contains("birth_")) |>
      select(where(function(x) {
        !all(is.na(x))
        }))

    # Join with dates of elections
    candidates <-
      candidates |>
      left_join(dates_elections_spain |> select(-topic),
                by = c("cod_elec", "type_elec", "year", "month"))  |>
      select(-year, -month, -day)

    # Rename and relocate
    candidates <-
      candidates |>
      rename(date_elec = date) |>
      relocate(date_elec, .after = type_elec)

    # output
    return(candidates)
  }

#' @rdname import_raw_elections_files
#' @export
import_raw_mun_MIR_files <-
  function(type_elec, year, month,
           base_url =
             "https://infoelectoral.interior.gob.es/estaticos/docxl/apliextr/",
           encoding = "Latin1",
           starts = c(1, 3, 7, 10, 12, 14, 17, 19, 120, 123,
                      129, 137, 142, 150, 158),
           ends = c(2, 6, 8, 11, 13, 16, 18, 118, 122, 125,
                    136, 141, 149, 157, 165)) {

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
      glue("{tempdir()}/05{cod_elec}{str_sub(year, start = 3, end = 4)}{char_month}.DAT")
    raw_file <-
      as_tibble(read_lines(file = path,
                           locale = locale(encoding = encoding)))

    # Delete temporary dir
    try(file.remove(list.files(tempdir(), full.names = TRUE, recursive = TRUE)),
        silent = TRUE)

    # Process variables following the instructions of register
    mun_file <-
      raw_file |>
      mutate(cod_elec = str_sub(value, start = starts[1], end = ends[1]),
             type_elec = type_elec,
             year =
               as.numeric(str_sub(value, starts[2], end = ends[2])),
             month =
               as.numeric(str_sub(value, start = starts[3], end = ends[3])),
             cod_MIR_ccaa =
               str_sub(value, start = starts[4], end = ends[4]),
             cod_INE_prov =
               str_sub(value, start = starts[5], end = ends[5]),
             cod_INE_mun =
               str_sub(value, start = starts[6], end = ends[6]),
             cod_mun_district =
               str_sub(value, start = starts[7], end = ends[7]),
             mun = str_trim(str_sub(value, start = starts[8], end = ends[8])),
             cod_mun_jud_district =
               str_trim(str_sub(value, start = starts[9], end = ends[9])),
             # provincial council (diputacion)
             cod_mun_prov_council =
               str_sub(value, start = starts[10], end = ends[10]),
             # Census of people who are living (CER + CERA)
             pop_res_mun =
               as.numeric(str_sub(value, start = starts[11], end = ends[11])),
             n_poll_stations =
               as.numeric(str_sub(value, start = starts[12], end = ends[12])),
             # pop_res who are allowed to vote
             census_INE_mun =
               as.numeric(str_sub(value, start = starts[13], end = ends[13])),
             # census_ine after claims
             census_counting_mun =
               as.numeric(str_sub(value, start = starts[14], end = ends[14])),
             # census CERE (census of foreigners, just for EU)
             census_CERE_mun =
               as.numeric(str_sub(value, start = starts[15],
                                  end = ends[15]))) |>
      select(-value) |>
      select(where(function(x) { !all(is.na(x)) })) |>
      mutate(id_MIR_mun =
               glue("{cod_MIR_ccaa}-{cod_INE_prov}-{cod_INE_mun}")) |>
      relocate(id_MIR_mun, .before = cod_MIR_ccaa) |>
      relocate(n_poll_stations, .before = pop_res_mun)

    # Remove municipal (non electoral) district data
    mun_file <-
      mun_file |>
      filter(cod_mun_district == "99") |>
      distinct(mun, cod_INE_mun, .keep_all = TRUE) |>
      select(-cod_mun_district)

    # Join with dates of elections
    mun_file <-
      mun_file |>
      left_join(dates_elections_spain |> select(-topic),
                by = c("cod_elec", "type_elec", "year", "month")) |>
      relocate(date, .after = type_elec) |>
      rename(date_elec = date) |>
      select(-year, -month, -day)

    # output
    return(mun_file)
  }

#' @rdname import_raw_elections_files
#' @export
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


#' @rdname import_raw_elections_files
#' @export
import_raw_candidacies_poll_file <-
  function(type_elec, year, month,
           base_url =
             "https://infoelectoral.interior.gob.es/estaticos/docxl/apliextr/",
           encoding = "Latin1",
           starts = c(1, 3, 7, 9, 10, 12, 14, 17, 19, 23, 24, 30),
           ends = c(2, 6, 8, 9, 11, 13, 16, 18, 22, 23, 29, 36)) {

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
      glue("{tempdir()}/10{cod_elec}{str_sub(year, start = 3, end = 4)}{char_month}.DAT")
    raw_file <-
      as_tibble(read_lines(file = path,
                           locale = locale(encoding = encoding)))

    # Delete temporary dir
    try(file.remove(list.files(tempdir(), full.names = TRUE, recursive = TRUE)),
        silent = TRUE)

    # Process variables following the instructions of register
    poll_stations <-
      raw_file |>
      mutate(cod_elec = str_sub(value, start = starts[1], end = ends[1]),
             type_elec = type_elec,
             year = as.numeric(str_sub(value, starts[2], end = ends[2])),
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
             id_MIR_mun =
               glue("{cod_MIR_ccaa}-{cod_INE_prov}-{cod_INE_mun}"),
             cod_mun_district =
               str_trim(str_sub(value, start = starts[8], end = ends[8])),
             cod_sec =
               str_trim(str_sub(value, start = starts[9], end = ends[9])),
             cod_poll_station =
               str_trim(str_sub(value, start = starts[10], end = ends[10])),
             id_candidacies =
               str_trim(str_sub(value, start = starts[11], end = ends[11])),
             ballots =
               as.numeric(str_sub(value, start = starts[12],
                                  end = ends[12]))) |>
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

    # output
    return(poll_stations)
  }

#' @title Recode of party's names
#'
#' @description ...
#'
#' @param parties_data ...
#' @param col_name_abbrev ...
#' @param col_name_candidacies ...
#'
#' @return ...
#'
#' @author Javier Álvarez-Liébana.
#' @keywords utils
#' @name recod_parties
#' @export
recod_parties <-
  function(parties_data, col_name_abbrev = "abbrev_candidacies",
           col_name_candidacies = "name_candidacies") {

    # Check: if col_name_abbrev and col_name_candidacies are characters
    if (!is.character(col_name_abbrev) ||
        !is.character(col_name_candidacies)) {

      stop("Parameters 'col_name_abbrev' and 'col_name_candidacies' must be character")
    }
    # Check: if parties_data contains at least two required columns
    if (!(col_name_abbrev %in% names(parties_data))) {

      stop("Data must contain (at least) a column with abbrev of candidacies")

    }

    # Rename
    parties_data <-
      parties_data |>
      rename(abbrev_candidacies = col_name_abbrev)

    # Recode
    parties_data <-
      parties_data |>
      mutate(
      # Remove ' and . and ,. Trimming. Reformat -
      abbrev_candidacies = str_remove_all(abbrev_candidacies, "'|\\.|\\,|´"),
      abbrev_candidacies = str_remove_all(abbrev_candidacies, '\\"'),
      abbrev_candidacies = str_trim(abbrev_candidacies),
      abbrev_candidacies = str_replace_all(abbrev_candidacies, "–| - |/", "-"),
      abbrev_candidacies = str_replace_all(abbrev_candidacies, "\\+", ""),
      # Remove tildes just from abbrev
      abbrev_candidacies =
        stri_trans_general(abbrev_candidacies, "Latin-ASCII"))

    # Preproc abbrev
    parties_data <-
      parties_data |>
      mutate(
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "EHBILDU|EH BILDU", "EH-BILDU"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "!TERUEL EXI", "TE"),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "AHORA CANARIAS"), "AHORA CANARIAS",
               abbrev_candidacies),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies,
                        "AP-PDP-PL-C|AP-PL-C|AP-PDP-PDL-|AP-PDP-PAR|AP-PDP-PDL|AP-PDP-PL|AP-PDP-UV|AP-PL-UPN|AP-PDP|AP-PAR|AP-PDL|AP-UV|AP-PL",
                        "AP"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "ARA, PV", "ARA-PV"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "BNG-NÓS|BNG-NOS|NÓS|NOS", "BNG"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "CC-PNC|CCA-PNC", "CC"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "NC-CCA-PNC|NC-CC-PNC|NC-CCA|NC-CC|CC-NC-PNC", "CC-NC"),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "COMPROMIS"),
               "COMPROMIS", abbrev_candidacies),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "AVANT ADELANTE LOS VERDES|AVANT LOS VERDES|GREENS|LOS VERDES|LV-LV|AVANT-LOS V|VERDES",
                        "LV"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "CUP-PR", "CUP"),
      abbrev_candidacies = ifelse(str_detect(abbrev_candidacies, "GREENS"),
                                  "LV", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(abbrev_candidacies == "DL", "DIL-CDC", abbrev_candidacies),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "M PAIS|MAS PAIS", "MP"),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies,
                          "PARTIT DELS SOCIALISTES DE CATALUNYA|PARTIDO DOS SOCIALISTAS DE GALICIA"),
               "PSOE", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies,
                          "PARTIDO SOCIALISTA OBRERO ESPAÑOL DE ANDALUCIA|PART. SOCIALISTA OBRERO ESPAÑOL DE ANDALUCIA"),
               "PSOE", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "PSE-EE|PARTIDO SOCIALISTA DE ARAGON|PARTIDO DOS SOCIALISTAS DE GALICIA"),
               "PSOE", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "PSOE-PROGR.|PSOE-PROGR|PSOE|PSC|PSE"),
               "PSOE", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "IULV-CA|ICV-EUIA"),
               "IU", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "PSA-PA"), "PA", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "ERC-CATSI|ERC"),
               "ERC", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "RUIZ-MATEOS"),
               "ARM", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "EA-EUE"), "EA",
               abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "NA\\+"), "NA-SUMA",
               abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "LIT-CI|LITCI"),
               "LIT-CI", abbrev_candidacies))

    # Preproc names
    if (col_name_candidacies %in% names(parties_data)) {

      # Rename
      parties_data <-
        parties_data |>
        rename(name_candidacies = col_name_candidacies)

      parties_data <-
        parties_data |>
        mutate(
          name_candidacies = str_replace_all(name_candidacies, "–| - |/", "-"),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "AHORA CANARIAS"), "AHORA CANARIAS",
                   abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(abbrev_candidacies, "DL") &
                     str_detect(name_candidacies, "LLIBERTAT"),
                   "DIL-CDC", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "LLUITA INTERNACIONALISTA"),
                   "LIT-CI", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "RECORTES CERO-GRUPO VERDE"),
                   "RECORTES CERO-LV", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "BERDEAK-LOS VERDES"),
                   "BERDEAK-LV", abbrev_candidacies),
          abbrev_candidacies = ifelse(str_detect(name_candidacies, "GREENS"),
                                      "LV", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "UNIDAS PODEMOS|UNIDOS PODEMOS"),
                   "UP", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies,
                              "PARTIT DELS SOCIALISTES DE CATALUNYA|PARTIDO DOS SOCIALISTAS DE GALICIA"),
                   "PSOE", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies,
                              "PARTIDO SOCIALISTA OBRERO ESPAÑOL DE ANDALUCIA|PART. SOCIALISTA OBRERO ESPAÑOL DE ANDALUCIA"),
                   "PSOE", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "PSE-EE|PARTIDO SOCIALISTA DE ARAGON|PARTIDO DOS SOCIALISTAS DE GALICIA"),
                   "PSOE", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(abbrev_candidacies, "PSOE-PROGR.|PSOE-PROGR"),
                   "PSOE", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "PARTIDO SOCIALISTA DE ANDALUCIA-PARTIDO ANDALUZ"),
                   "PA", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies,
                              "ESQUERRA REPUBLICANA DE CATALUNYA"),
                   "ERC", abbrev_candidacies),
          name_candidacies =
            ifelse(str_detect(name_candidacies, "UNIDAS PODEMOS|UNIDOS PODEMOS"),
                   "UNIDOS PODEMOS", name_candidacies),
          name_candidacies =
            ifelse(abbrev_candidacies == "AP",
                   "ALIANZA POPULAR-COALICION POPULAR", name_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "NAVARRA SUMA"), "NA-SUMA",
                   abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "RUIZ-MATEOS"),
                   "ARM", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies,
                              "INICIATIVA PER CATALUNYA-ELS VERDS"),
                   "ICV", abbrev_candidacies))

    }

    return(parties_data)

}
