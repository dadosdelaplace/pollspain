#' @title Import municipal census data
#'
#' @description Import municipal census data for one or more elections
#' at the municipal level. This function downloads and processes raw
#' municipal data files for specified elections.
#'
#' @inheritParams type_to_code_election
#' @param year A vector or single value representing the years of the
#' elections to be considered.
#' @param month A vector or single value representing the months of
#' the elections to be considered. Please, check (refer to
#' \code{dates_elections_spain}) that elections of the specified type
#' are available for the provided year and month.
#' @param date A vector or single value representing the dates of
#' the elections to be considered. If date was provided, it should be
#' in format %Y-%m-%d (e.g., '2000-01-01'). Defaults to \code{NULL}.
#' If no date was provided, \code{month} and \code{year} should be
#' both numerical variables.
#' @param repo_url A string with the url in which the raw data can be
#' found. Defaults to
#' \url{https://github.com/dadosdelaplace/pollspain-data/blob/main}.
#' @param file_ext A string with the file's extension. Defaults to
#' \code{".rda"}.
#' @param verbose Flag to indicate whether detailed messages should
#' be printed during execution. Defaults to \code{TRUE}.
#'
#' @return A tibble with rows corresponding to municipalities for
#' each election, including the following variables:
#' \item{id_elec}{election's id constructed from the election code
#' \code{cod_elec} and date \code{date_elec}.}
#' \item{cod_elec}{code representing the type of election: \code{"01"}
#' (referendum), \code{"02"} (congress), \code{"03"} (senate),
#' \code{"04"} (local elections), \code{"06"} (cabildo - Canarian
#' council - elections), \code{"07"} (European Parliament elections).}
#' \item{type_elec}{type of election.}
#' \item{date_elec}{date of the election.}
#' \item{id_INE_mun}{municipality ID constructed from the
#' ccaa-prov-mun codes provided by INE.}
#' \item{cod_INE_ccaa, ccaa}{codes and names for regions (ccaa)
#' to which the municipalities belong.}
#' \item{cod_INE_prov, prov}{codes and names for the provinces to
#' which the municipalities belong.}
#' \item{cod_INE_mun, mun}{code, and name for municipalities.}
#' \item{cod_mun_jud_district, cod_mun_prov_council}{codes for the
#' judicial district and provincial council.}
#' \item{n_poll_stations}{number of polling stations in each
#' municipality.}
#' \item{pop_res_mun}{population census of residents (CER + CERA)
#' at municipality level.}
#' \item{census_INE_mun}{population eligible to vote at municipality
#' level.}
#' \item{census_counting_mun}{population eligible to vote after
#' claims at municipality level.}
#' \item{census_CERE_mun}{census of foreign nationals, relevant only
#' for EU elections at municipality level.}
#'
#' @details This function fetches municipal-level data for the
#' specified elections by downloading the corresponding `.rda` files
#' from GitHub \url{https://github.com/dadosdelaplace/pollspain-data}
#' (directly downloaded from MIR website) and processing them into a
#' tidy format. It automatically handles the download, loading, and
#' merging of data across multiple election periods as specified by
#' the user.
#'
#' @author Javier Álvarez-Liébana, David Pereiro Pol, Mafalda González
#' González, Irene Bosque Gala and Mikaela De Smedt.
#' @source Some definitions of variables were extracted from
#' \url{https://www.ige.gal}.
#' @keywords import_elections_data
#' @name import_mun_census_data
#' @import crayon
#' @examples
#'
#' ## Correct examples
#'
#' # Congress elections in April 2019
#' # Fetch municipal census data for the congress elections in
#' # April 2019
#' mun_census_data1 <- import_mun_census_data("congress", 2019, 4)
#'
#' # Senate elections in April 2019
#' # Fetch municipal census data for the senate elections in
#' # April 2019
#' mun_census_data2 <- import_mun_census_data("senate", 2019, 4)
#'
#' # Example usage to combine data from different elections into
#' # one table
#' # Fetch municipal census data for congress elections in Nov 2019
#' # and June 2016
#' combined_mun_census_data <-
#'   import_mun_census_data(c("congress", "congress"),
#'                          c(2019, 2016), c(11, 6))
#'
#' # Example usage providing date instead of month and year
#' date_example <-
#'   import_mun_census_data("congress", date = "2016-06-26")
#'
#' # ----
#' # Incorrect examples
#' # ----
#' \dontrun{
#' # Wrong examples
#'
#' # Invalid election type: "national" is not a valid election type
#' import_mun_census_data("national", 2019, 4)
#'
#' # Invalid election: no congress elections are available in 5-2019.
#' # Please check dataset dates_elections_spain
#' import_mun_census_data("congress", 2019, 5)
#'
#' # Invalid date format: date should be in %Y-%m-%d format
#' import_mun_census_data("congress", date = "26-06-2016")
#' }
#' @export
import_mun_census_data <-
  function(type_elec, year = 2019, month = 4, date = NULL,
           repo_url = "https://github.com/dadosdelaplace/pollspain-data/blob/main",
           file_ext = ".rda", verbose = TRUE) {

    # Check if verbose is correct
    if (!is.logical(verbose) | is.na(verbose)) {

      stop(red("😵 `verbose` argument should be a TRUE/FALSE logical flag."))


    }

    if (verbose) {

      message(yellow("🔎 Check if parameters are allowed..."))
      Sys.sleep(1/20)

    }

    # for the moment, just congress election
    if (!all(type_elec %in% c("congress", "senate"))) {

      stop(red("😵 The package is currently under development so, for the moment, it only allows access to congress and senate data."))

    }

    # Check if the inputs are vectors of the same length
    if (!is.vector(type_elec) | !is.vector(year) | !is.vector(month)) {

      stop(red("😵 All inputs must be vectors."))

    }

    # check if date is correct
    if (is.null(date)) {
      if (!is.numeric(month) | !is.numeric(year)) {
        stop(red("😵 If no date was provided, `month` and `year` should be both numerical variables."))
      }
    } else {

      date <- as_date(date)
      if (is.na(date)) {
        stop(red("😵 If date was provided, `date` should be in format '2000-01-01' (%Y-%m-%d)"))
      }
    }

    # Design a tibble with all elections asked by user
    # Ensure input parameters are vectors
    if (is.null(date)) {

      asked_elections <-
        expand_grid(as.vector(type_elec), as.vector(month), as.vector(year))
      names(asked_elections) <- c("type_elec", "month", "year")
      asked_elections <-
        asked_elections |>
        distinct(type_elec, month, year)

    } else {

      asked_elections <-
        expand_grid(as.vector(type_elec), date) |>
        mutate("month" = month(date), "year" = year(date))
      names(asked_elections) <- c("type_elec", "date", "month", "year")
      asked_elections <-
        asked_elections |>
        distinct(type_elec, date, month, year)
    }

    asked_elections <-
      asked_elections |>
      mutate("cod_elec" = type_to_code_election(as.vector(type_elec)),
             .before = everything())

    # Check if elections required are allowed
    allowed_elections <-
      dates_elections_spain |>
      inner_join(asked_elections,
                 by = c("cod_elec", "type_elec", "year", "month")) |>
      distinct(cod_elec, type_elec, year, month, .keep_all = TRUE)
    if (allowed_elections |> nrow() == 0) {

      stop(red(glue("😵 No {type_elec} elections are available in {month}-{year}. Please, be sure that the order of arguments is right")))

    }

    # Construct the set of URL for the specific election directory
    dirs <- glue("{allowed_elections$cod_elec}-{allowed_elections$type_elec}")
    dirs <- glue("{dirs}/{allowed_elections$cod_elec}{allowed_elections$year}{sprintf('%02d', allowed_elections$month)}")
    elections_dir <- glue("{repo_url}/{dirs}")
    files_url <- glue("{elections_dir}/raw_mun_data_{allowed_elections$type_elec}_{allowed_elections$year}_{sprintf('%02d', allowed_elections$month)}.rda?raw=true")

    if (verbose) {

      # Print the file URL for debugging purposes
      message(blue("📦 Import census mun data from ..."))
      Sys.sleep(1/20)
      message(green(glue("   - {paste0(files_url, '\n')}")))

    }

    wrong_elections <-
      asked_elections |>
      anti_join(dates_elections_spain,
                by = c("cod_elec", "type_elec", "year", "month"))

    if (verbose) {
      if ((wrong_elections |> nrow() > 0) &
          (allowed_elections |> nrow() == 0)) {

        message(yellow(glue("⚠️ No data is available for {wrong_elections$type_elec} elections in {paste0(wrong_elections$month, '-', wrong_elections$year, '\n')}")))

      }
    }

    # download files in a temp_folder
    temp_files <- replicate(n = length(files_url),
                            expr = tempfile(fileext = file_ext))

    # loop to avoid rmd error when examples are generated
    for (i in 1:length(files_url)){

      download.file(files_url[i], destfile = temp_files[i], mode = "wb", quiet = TRUE)
    }

    # Check the downloaded file before loading
    if (any(file.info(temp_files)$size == 0)) {

      stop(red("😵 Any of downloaded files is empty. Please, be sure that you are connected to the internet."))

    }

    # Import the data
    mun_data <-
      temp_files |>
      map(function(x) { get(load(x)) }) |>
      list_rbind()

    # Join MIR and INE information
    mun_data <-
      mun_data |>
      left_join(cod_INE_mun, by = c("id_MIR_mun", "cod_MIR_ccaa",
                                    "cod_INE_prov", "cod_INE_mun"),
                suffix = c(".x", "")) |>
      # Keep names from cod INE files instead of MIR files
      dplyr::select(-mun.x, -day, -year, -month, -cd_INE_mun, -contains("MIR")) |>
      # create id_elec
      mutate("id_elec" = glue("{cod_elec}-{date_elec}"),
             .before = everything()) |>
      # Relocate
      relocate(id_INE_mun, .after = date_elec) |>
      relocate(cod_INE_ccaa, .after = id_INE_mun) |>
      relocate(mun, .after = cod_INE_mun) |>
      relocate(ccaa, .after = cod_INE_ccaa) |>
      relocate(prov, .after = cod_INE_prov)

    # output
    return(mun_data)

  }


#' @title Import poll station data
#'
#' @description Import and preprocess elections data at poll stations
#' level for given election types and dates. This function supports
#' both single values and vector inputs for fetching and combining
#' data for multiple elections at once.
#'
#' @inheritParams import_mun_census_data
#' @param prec_round Rounding accuracy. Defaults to
#' \code{prec_round = 3}.
#' @param short_version Flag to indicate whether it should be returned
#' a short version of the data (just key variables) or not.
#' Defaults to \code{TRUE}.
#'
#' @return A tibble with rows corresponding to municipalities for
#' each election, including the following variables:
#' \item{id_elec}{election's id constructed from the election code
#' \code{cod_elec} and date \code{date_elec}.}
#' \item{cod_elec}{code representing the type of election:
#' \code{"01"} (referendum), \code{"02"} (congress),
#' \code{"03"} (senate), \code{"04"} (local elections),
#' \code{"06"} (cabildo - Canarian council - elections), \code{"07"}
#' (European Parliament elections). Variable available only for
#' long version.}
#' \item{type_elec}{type of election.}
#' \item{date_elec}{date of the election.}
#' \item{id_INE_mun}{municipality ID constructed from the
#' ccaa-prov-mun codes provided by INE.}
#' \item{id_INE_poll_station}{poll station's id constructed from the
#' ccaa-prov-municipality and poll station codes.}
#' \item{cod_INE_ccaa, ccaa}{codes and names for regions (ccaa)
#' to which the municipalities belong. Codes available only for
#' long version.}
#' \item{cod_INE_prov, prov}{codes and names for provinces to which
#' municipalities belong. Codes available only for long version.}
#' \item{cod_INE_mun, mun}{code, and name for
#' municipalities. Codes available only for long version.}
#' \item{cod_mun_district, cod_sec, cod_poll_station}{codes for the
#' municipal district, census tract and poll station. Codes available
#' only for long version.}
#' \item{census_counting_mun}{population eligible to vote after claims
#' at municipality level.}
#' \item{ballots_1, turnout_1}{number of total ballots and turnout
#' percentage in the first round. Variables available only for
#' long version.}
#' \item{ballots_2, turnout_2}{number of total ballots and turnout
#' percentage in the second round (if applicable). Variables available
#' only for long version}
#' \item{blank_ballots, invalid_ballots}{blank and invalid ballots.}
#' \item{party_ballots, valid_ballots, total_ballots}{ballots to
#' candidacies/parties, valid ballots (sum of \code{blank_ballots} and
#' \code{party_ballots}) and total ballots (sum of
#' \code{valid_ballots} and \code{invalid_ballots}).}
#' \item{turnout}{final turnout percentage.}
#' \item{porc_valid, porc_invalid, porc_parties, porc_blank}{perc (%)
#' values of \code{valid_ballots}, \code{invalid_ballots},
#' \code{party_ballots} and \code{blank_ballots}.}
#' \item{pop_res_mun}{population census of residents (CER + CERA) at
#' municipality level.}
#'
#' @details This function fetches poll station-level data for the
#' specified elections by downloading the corresponding `.rda` files
#' from GitHub \url{https://github.com/dadosdelaplace/pollspain-data}
#' (directly downloaded from MIR website) and processing them into a
#' tidy format. It automatically handles the download, loading, and
#' merging of data across multiple election periods as specified by
#' the user.
#'
#' @author Javier Álvarez-Liébana, David Pereiro Pol, Mafalda González
#' González, Irene Bosque Gala and Mikaela De Smedt.
#' @keywords import_elections_data
#' @name import_poll_station_data
#'
#' @examples
#'
#' ## Correct examples
#'
#' # Congress elections in April 2019
#' # Fetch poll station data for the congress elections in April 2019
#' # in a short version
#' poll_station_data1 <- import_poll_station_data("congress", 2019, 4)
#'
#' #' # Fetch poll station data for the congress and senate elections
#' # in April 2019 in a short version
#' poll_station_data2 <-
#'   import_poll_station_data(c("congress", "senate"), 2019, 4)
#'
#' # Fetch poll station data for the congress and senate elections in
#' # April 2019 in a long version
#' poll_station_data3 <-
#'   import_poll_station_data(c("congress", "senate"), 2019, 4,
#'                            short_version = FALSE)
#'
#' # Fetch poll station data for congress elections in Nov 2019
#' # and June 2016
#' combined_poll_station_data <-
#'   import_poll_station_data(c("congress", "congress"),
#'                            c(2019, 2016), c(11, 6))
#'
#' # Example usage providing date instead of month and year
#' date_example <-
#'   import_poll_station_data("congress", date = "2016-06-26")
#'
#' # ----
#' # Incorrect examples
#' # ----
#'
#' \dontrun{
#' # Wrong examples
#'
#' # Invalid election type: "national" is not a valid election type
#' import_poll_station_data("national", 2019, 4)
#'
#' # Invalid election: no congress elections are available in 5-2019.
#' # Please check dataset dates_elections_spain
#' import_poll_station_data("congress", 2019, 5)
#'
#' # Invalid date format: date should be in %Y-%m-%d format
#' import_poll_station_data("congress", date = "26-06-2016")
#'
#' # Invalid short version flag: short_version should be a logical
#' # variable
#' import_poll_station_data("congress", 2019, 4,
#'                          short_version = "yes")
#' }
#'
#' @export
import_poll_station_data <-
  function(type_elec, year = 2019, month = 4, date = NULL,
           prec_round = 3, short_version = TRUE,
           repo_url = "https://github.com/dadosdelaplace/pollspain-data/blob/main",
           file_ext = ".rda", verbose = TRUE) {

    # Check if verbose is correct
    if (!is.logical(verbose) | is.na(verbose)) {

      stop(red("😵 `verbose` argument should be a TRUE/FALSE logical flag."))


    }

    if (verbose) {

      message(yellow("🔎 Check if parameters are allowed..."))
      Sys.sleep(1/20)

    }

    # for the moment, just congress and senate election
    if (!all(type_elec %in% c("congress", "senate"))) {

      stop(red("😵 The package is currently under development so, for the moment, it only allows access to congress and senate data."))

    }

    # Check if the inputs are vectors of the same length
    if (!is.vector(type_elec) | !is.vector(year) | !is.vector(month)) {

      stop(red("😵 All inputs must be vectors."))

    }

    # Check if prec_round is a positive number
    if (prec_round != as.integer(prec_round) | prec_round < 1) {

      stop(red("😵 Parameter 'prec_round' must be a positive integer greater than 0"))

    }

    # check if short_version is a logical variable
    if (is.na(short_version) | !is.logical(short_version)) {

      stop(red("😵 Parameter 'short_version' must be a non missing logical variable"))
    }

    # check if date is correct
    if (is.null(date)) {
      if (!is.numeric(month) | !is.numeric(year)) {
        stop(red("😵 If no date was provided, `month` and `year` should be both numerical variables."))
      }
    } else {

      date <- as_date(date)
      if (is.na(date)) {
        stop(red("😵 If date was provided, `date` should be in format '2000-01-01' (%Y-%m-%d)"))
      }
    }

    # Design a tibble with all elections asked by user
    # Ensure input parameters are vectors
    if (is.null(date)) {

      asked_elections <-
        expand_grid(as.vector(type_elec), as.vector(month), as.vector(year))
      names(asked_elections) <- c("type_elec", "month", "year")
      asked_elections <-
        asked_elections |>
        distinct(type_elec, month, year)

    } else {

      asked_elections <-
        expand_grid(as.vector(type_elec), date) |>
        mutate("month" = month(date), "year" = year(date))
      names(asked_elections) <- c("type_elec", "date", "month", "year")
      asked_elections <-
        asked_elections |>
        distinct(type_elec, date, month, year)
    }

    asked_elections <-
      asked_elections |>
      mutate("cod_elec" = type_to_code_election(as.vector(type_elec)),
             .before = everything())

    # Check if elections required are allowed
    allowed_elections <-
      dates_elections_spain |>
      inner_join(asked_elections,
                 by = c("cod_elec", "type_elec", "year", "month")) |>
      distinct(cod_elec, type_elec, year, month, .keep_all = TRUE)
    if (allowed_elections |> nrow() == 0) {

      stop(red(glue("😵 No {type_elec} elections are available in {month}-{year}. Please, be sure that the order of arguments is right")))

    }

    # Construct the set of URL for the specific election directory
    dirs <- glue("{allowed_elections$cod_elec}-{allowed_elections$type_elec}")
    dirs <- glue("{dirs}/{allowed_elections$cod_elec}{allowed_elections$year}{sprintf('%02d', allowed_elections$month)}")
    elections_dir <- glue("{repo_url}/{dirs}")
    files_url <- glue("{elections_dir}/raw_poll_stations_{allowed_elections$type_elec}_{allowed_elections$year}_{sprintf('%02d', allowed_elections$month)}.rda?raw=true")

    if (verbose) {

      # Print the file URL for debugging purposes
      message(blue("📦 Import poll station data from ..."))
      Sys.sleep(1/20)
      message(green(glue("   - {paste0(files_url, '\n')}")))

    }

    wrong_elections <-
      asked_elections |>
      anti_join(dates_elections_spain,
                by = c("cod_elec", "type_elec", "year", "month"))

    if (verbose) {
      if ((wrong_elections |> nrow() > 0) &
          (allowed_elections |> nrow() == 0)) {

        message(yellow(glue("⚠️ No data is available for {wrong_elections$type_elec} elections in {paste0(wrong_elections$month, '-', wrong_elections$year, '\n')}")))

      }
    }

    # download files in a temp_folder
    temp_files <- replicate(n = length(files_url),
                            expr = tempfile(fileext = file_ext))

    # loop to avoid rmd error when examples are generated
    for (i in 1:length(files_url)){

      download.file(files_url[i], destfile = temp_files[i], mode = "wb", quiet = TRUE)
    }

    # Check the downloaded file before loading
    if (any(file.info(temp_files)$size == 0)) {

      stop(red("😵 Any of downloaded files is empty. Please, be sure that you are connected to the internet."))

    }

    # Import the data
    poll_station_raw_data <-
      temp_files |>
      map(function(x) { get(load(x)) }) |>
      list_rbind() |>
      # census variable will be extracted from the mun census
      select(-contains("census"))

    # Join MIR and INE information
    poll_station_data <-
      poll_station_raw_data |>
      left_join(cod_INE_mun, by = c("id_MIR_mun", "cod_MIR_ccaa",
                                    "cod_INE_prov", "cod_INE_mun"),
                suffix = c(".x", "")) |>
      dplyr::select(-cd_INE_mun) |>
      # create id_elec
      mutate("id_elec" = glue("{cod_elec}-{date_elec}"),
             .before = everything())

    # Some basic statistics
    poll_station_data <-
      poll_station_data |>
      mutate("valid_ballots" = blank_ballots + party_ballots,
             "total_ballots" = valid_ballots + invalid_ballots)

    # Remove CERA data and include census
    poll_station_data <-
      poll_station_data |>
      dplyr::filter(cod_INE_mun != "999") |>
      left_join(import_mun_census_data(type_elec, year, month, date, repo_url, file_ext,
                                       verbose = verbose),
                by = c("cod_elec", "type_elec", "date_elec", "id_INE_mun"),
                suffix = c("", ".y")) |>
      select(-contains(".y"))


    # Relocate and rename columns
    poll_station_data <-
      poll_station_data |>
      relocate(id_INE_mun, .after = date_elec) |>
      relocate(cod_INE_ccaa, .after = id_INE_mun) |>
      relocate(ccaa, .after = cod_INE_ccaa) |>
      relocate(prov, .after = cod_INE_prov) |>
      relocate(mun, .after = cod_INE_mun)

    # Include CERA data and their ccaa and prov
    poll_station_data <-
      poll_station_data |>
      bind_rows(poll_station_raw_data |> filter(cod_INE_mun == "999")) |>
      left_join(cod_INE_mun |>
                  distinct(cod_MIR_ccaa, cod_INE_prov, .keep_all = TRUE) |>
                  select(contains("ccaa") | contains("prov")),
                by = c("cod_MIR_ccaa", "cod_INE_prov"),
                suffix = c("", ".y")) |>
      # debugging codes
      # we need to create again id_elec for those records
      mutate("cod_INE_ccaa" =
               ifelse(is.na(cod_INE_ccaa), cod_INE_ccaa.y, cod_INE_ccaa),
             "ccaa" = ifelse(is.na(ccaa), ccaa.y, ccaa),
             "prov" = ifelse(is.na(prov), prov.y, prov),
             "mun" = ifelse(cod_INE_mun == "999", "CERA", mun),
             "id_INE_mun" = glue("{cod_INE_ccaa}-{cod_INE_prov}-{cod_INE_mun}"),
             "pop_res_mun" = ifelse(cod_INE_mun == "999", census_INE_mun, pop_res_mun),
             "id_elec" = glue("{cod_elec}-{date_elec}")) |>
      # Remove variables
      select(-contains(".y"),
             -c(census_CERE_mun, voters_cere, census_INE_mun, turn,
                cod_mun_jud_district, cod_mun_prov_council))

    # Include turnout data (use census_counting from poll stations files)
    poll_station_data <-
      poll_station_data |>
      # Remove MIR codes
      select(-contains("MIR")) |>
      drop_na(id_INE_mun) |>
      mutate("id_INE_poll_station" =
               glue("{id_INE_mun}-{cod_mun_district}-{cod_sec}-{cod_poll_station}"),
             "turnout_1" = round(100 * ballots_1 / census_counting_mun, prec_round),
             "turnout_2" = round(100 * ballots_2 / census_counting_mun, prec_round),
             "turnout" = round(100 * total_ballots / census_counting_mun, prec_round),
             # % valid and invalid ballots over total ballots
             "porc_valid" =
               round(100 * valid_ballots / total_ballots, prec_round),
             "porc_invalid" =
               round(100 * invalid_ballots / total_ballots, prec_round),
             # % party and blank ballots over valid ballots
             "porc_parties" =
               round(100 * party_ballots / valid_ballots, prec_round),
             "porc_blank" =
               round(100 * blank_ballots / valid_ballots, prec_round)) |>
      relocate(turnout:porc_blank, .after = total_ballots) |>
      relocate(id_INE_poll_station, .after = date_elec) |>
      relocate(turnout_1, .after = ballots_1) |>
      relocate(turnout_2, .after = ballots_2)

    if (short_version) {

      if (verbose) {

        message(yellow("⚠️ A short version was asked. If you require all variables, please run with `short_version = FALSE'"))

      }

      # Select just few variables
      poll_station_data <-
        poll_station_data |>
        select(id_elec, type_elec, date_elec, id_INE_poll_station,
               id_INE_mun, ccaa, prov, mun, blank_ballots,
               invalid_ballots, party_ballots, valid_ballots,
               total_ballots, turnout, porc_valid, porc_invalid,
               porc_parties, porc_blank, pop_res_mun, census_counting_mun)
    }
    # output
    return(poll_station_data)
  }


#' @title Import candidacies data
#'
#' @description Import and preprocess candidacies data. This function
#' supports both single values and vector inputs for fetching and
#' combining data for multiple elections at once.
#'
#' @inheritParams import_mun_census_data
#' @param short_version flag to indicate whether it should be returned
#' a short version of the data (just key  variables)
#' or not. Defaults to \code{TRUE}.
#'
#' @return A tibble with candidacies data at poll station level
#' including the following variables:
#' \item{id_elec}{election's id constructed from the election code
#' \code{cod_elec} and date \code{date_elec}.}
#' \item{cod_elec}{code representing the type of election:
#' \code{"01"} (referendum), \code{"02"} (congress),
#' \code{"03"} (senate), \code{"04"} (local elections),
#' \code{"06"} (cabildo - Canarian council - elections), \code{"07"}
#' (European Parliament elections). Variable available only for
#' long version.}
#' \item{type_elec}{type of election.}
#' \item{date_elec}{date of the election.}
#' \item{id_INE_poll_station}{poll station's id constructed from the
#' ccaa-prov-municipality and poll station codes.}
#' \item{id_INE_mun}{municipality ID constructed from the
#' ccaa-prov-mun codes provided by INE.}
#' \item{cod_INE_ccaa, ccaa}{codes and names for regions (ccaa)
#' to which the municipalities belong. Codes available only for
#' long version.}
#' \item{cod_INE_prov, prov}{codes and names for the provinces to
#' which the municipalities belong. Codes available only for
#' long version.}
#' \item{cod_INE_mun, mun}{code, and name for
#' municipalities. Codes available only for
#' long version.}
#' \item{cod_mun_district, cod_sec, cod_poll_station}{codes for the
#' municipal district, census tract and poll station. Codes available
#' only for long version.}
#' \item{id_candidacies}{id for candidacies (at province level).}
#' \item{id_candidacies_ccaa, id_candidacies_nat}{id for
#' candidacies (at region - ccaa - and national level). Id's available
#' only for long version.}
#' \item{abbrev_candidacies, name_candidacies}{acronym and full name
#' of the candidacies.}
#' \item{ballots}{number of ballots obtained for each candidacy at
#' each poll station.}
#'
#' @details This function fetches candidates data for the
#' specified elections by downloading the corresponding `.rda` files
#' from GitHub \url{https://github.com/dadosdelaplace/pollspain-data}
#' (directly downloaded from MIR website) and processing them into a
#' tidy format. It automatically handles the download, loading, and
#' merging of data across multiple election periods as specified by
#' the user.
#'
#' @author Javier Álvarez-Liébana, David Pereiro Pol, Mafalda González
#' González, Irene Bosque Gala and Mikaela De Smedt.
#' @keywords import_elections_data
#' @name import_candidacies_data
#'
#' @examples
#'
#' ## Correct examples
#'
#' # Congress elections in April 2019
#' # Fetch candidacies data for the congress elections in April 2019
#' candidacies_data1 <- import_candidacies_data("congress", 2019, 4)
#'
#' # Fetch candidacies data for the congress and senate elections
#' # in April 2019
#' candidacies_data2 <-
#'   import_candidacies_data(c("congress", "senate"), 2019, 4)
#'
#' # Example usage to combine data from different elections into
#' # one table
#' # Fetch candidacies data for congress elections in Nov 2019
#' # and June 2016
#' combined_candidacies_data <-
#'   import_candidacies_data(c("congress", "congress"),
#'                           c(2019, 2016), c(11, 6))
#'
#' # Example usage providing date instead of month and year
#' date_example <-
#'   import_candidacies_data("congress", date = "2016-06-26")
#'
#' # ----
#' # Incorrect examples
#' # ----
#'
#' \dontrun{
#' # Wrong examples
#'
#' # Invalid election type: "national" is not a valid election type
#' import_candidacies_data("national", 2019, 4)
#'
#' # Invalid election: no congress elections are available in 5-2019.
#' # Please check dataset dates_elections_spain
#' import_candidacies_data("congress", 2019, 5)
#'
#' # Invalid date format: date should be in %Y-%m-%d format
#' import_candidacies_data("congress", date = "26-06-2016")
#'
#' }
#'
#' @export
import_candidacies_data <-
  function(type_elec, year = 2019, month = 4, date = NULL,
           repo_url = "https://github.com/dadosdelaplace/pollspain-data/blob/main",
           file_ext = ".rda", short_version = TRUE, verbose = TRUE) {

    # Check if verbose is correct
    if (!is.logical(verbose) | is.na(verbose)) {

      stop(red("😵 `verbose` argument should be a TRUE/FALSE logical flag."))


    }

    if (verbose) {

      message(yellow("🔎 Check if parameters are allowed..."))
      Sys.sleep(1/20)

    }

    # for the moment, just congress and senate election
    if (!all(type_elec %in% c("congress", "senate"))) {

      stop(red("😵 The package is currently under development so, for the moment, it only allows access to congress and senate data."))

    }

    # Check if the inputs are vectors of the same length
    if (!is.vector(type_elec) | !is.vector(year) | !is.vector(month)) {

      stop(red("😵 All inputs must be vectors."))

    }

    # check if date is correct
    if (is.null(date)) {
      if (!is.numeric(month) | !is.numeric(year)) {
        stop(red("😵 If no date was provided, `month` and `year` should be both numerical variables."))
      }
    } else {

      date <- as_date(date)
      if (is.na(date)) {
        stop(red("😵 If date was provided, `date` should be in format '2000-01-01' (%Y-%m-%d)"))
      }
    }

    # Design a tibble with all elections asked by user
    # Ensure input parameters are vectors
    if (is.null(date)) {

      asked_elections <-
        expand_grid(as.vector(type_elec), as.vector(month), as.vector(year))
      names(asked_elections) <- c("type_elec", "month", "year")
      asked_elections <-
        asked_elections |>
        distinct(type_elec, month, year)

    } else {

      asked_elections <-
        expand_grid(as.vector(type_elec), date) |>
        mutate("month" = month(date), "year" = year(date))
      names(asked_elections) <- c("type_elec", "date", "month", "year")
      asked_elections <-
        asked_elections |>
        distinct(type_elec, date, month, year)
    }

    asked_elections <-
      asked_elections |>
      mutate("cod_elec" = type_to_code_election(as.vector(type_elec)),
             .before = everything())

    # Check if elections required are allowed
    allowed_elections <-
      dates_elections_spain |>
      inner_join(asked_elections,
                 by = c("cod_elec", "type_elec", "year", "month")) |>
      distinct(cod_elec, type_elec, year, month, .keep_all = TRUE)
    if (allowed_elections |> nrow() == 0) {

      stop(red(glue("😵 No {type_elec} elections are available in {month}-{year}. Please, be sure that the order of arguments is right")))

    }

    # Construct the set of URL for the specific election directory
    dirs <- glue("{allowed_elections$cod_elec}-{allowed_elections$type_elec}")
    dirs <- glue("{dirs}/{allowed_elections$cod_elec}{allowed_elections$year}{sprintf('%02d', allowed_elections$month)}")
    elections_dir <- glue("{repo_url}/{dirs}")
    files_url <- glue("{elections_dir}/raw_candidacies_poll_{allowed_elections$type_elec}_{allowed_elections$year}_{sprintf('%02d', allowed_elections$month)}.rda?raw=true")

    if (verbose) {

      # Print the file URL for debugging purposes
      message(blue("📦 Import candidacies data at poll station level from ..."))
      Sys.sleep(1/20)
      message(green(glue("   - {paste0(files_url, '\n')}")))

    }

    wrong_elections <-
      asked_elections |>
      anti_join(dates_elections_spain,
                by = c("cod_elec", "type_elec", "year", "month"))

    if (verbose) {
      if ((wrong_elections |> nrow() > 0) &
          (allowed_elections |> nrow() == 0)) {

        message(yellow(glue("⚠️ No data is available for {wrong_elections$type_elec} elections in {paste0(wrong_elections$month, '-', wrong_elections$year, '\n')}")))

      }

      message(magenta("⏳ Please wait, the volume of data downloaded and the internet connection may take a few seconds"))
    }

    # download files in a temp_folder
    temp_files <- replicate(n = length(files_url),
                            expr = tempfile(fileext = file_ext))

    # loop to avoid rmd error when examples are generated
    for (i in 1:length(files_url)){

      download.file(files_url[i], destfile = temp_files[i], mode = "wb", quiet = TRUE)
    }

    # Check the downloaded file before loading
    if (any(file.info(temp_files)$size == 0)) {

      stop(red("😵 Any of downloaded files is empty. Please, be sure that you are connected to the internet."))

    }

    # Import the data
    candidacies_raw_data <-
      temp_files |>
      map(function(x) { get(load(x)) }) |>
      list_rbind() |>
      # remove summarys in CERA
      filter(cod_MIR_ccaa != "99" & cod_INE_prov != "99")

    # include CERA codes to INE information: just one
    # constituency for each provinces
    cod_INE_mun_CERA <-
      cod_INE_mun |>
      distinct(cod_INE_ccaa, cod_INE_prov, .keep_all  = TRUE) |>
      select(cod_INE_ccaa, cod_MIR_ccaa, cod_INE_prov, ccaa, prov) |>
      mutate("mun" = "CERA", cod_INE_mun = "999") |>
      bind_rows(cod_INE_mun) |> # include "normal" mun
      mutate("id_INE_mun" =
               glue("{cod_INE_ccaa}-{cod_INE_prov}-{cod_INE_mun}"),
             "id_MIR_mun" =
               glue("{cod_MIR_ccaa}-{cod_INE_prov}-{cod_INE_mun}"),
             .before = everything())

    # Join MIR and INE information
    candidacies_data <-
      candidacies_raw_data  |>
      left_join(cod_INE_mun_CERA, by = "id_MIR_mun",
                suffix = c(".x", "")) |>
      # Keep names from cod INE files instead of MIR files
      select(-contains(".x")) |>
      # remove variables
      select(-cd_INE_mun, -turn) |>
      # Include id_INE_poll_station and id_elec
      mutate("id_INE_poll_station" =
               glue("{id_INE_mun}-{cod_mun_district}-{cod_sec}-{cod_poll_station}"),
             id_elec = glue("{cod_elec}-{date_elec}"))

    # Relocate
    candidacies_data <-
      candidacies_data |>
      relocate(id_elec, .before = everything()) |>
      relocate(id_INE_poll_station, .after = date_elec) |>
      relocate(id_INE_mun, .before = id_MIR_mun) |>
      relocate(cod_INE_ccaa, cod_MIR_ccaa, ccaa,
               cod_INE_prov, prov, cod_INE_mun, mun, .after = id_MIR_mun)

    # Join candidacies data
    # candidacies_data <-
    #   candidacies_data |>
    #   left_join(historical_raw_candidacies |>
    #               filter(type_elec %in% type_elec &
    #                        year(date_elec) %in% year &
    #                        month(date_elec) %in% month),
    #             by = c("cod_elec", "type_elec",
    #                    "date_elec", "id_candidacies"))

    # Include candidacies info
    files_url <- glue("{elections_dir}/raw_candidacies_{allowed_elections$type_elec}_{allowed_elections$year}_{sprintf('%02d', allowed_elections$month)}.rda?raw=true")

    # Print the file URL for debugging purposes
    # message(blue("ℹ️ Import candidacies info from ..."))
    # Sys.sleep(1/20)
    # message(green(glue("   - {paste0(files_url, '\n')}")))

    # download files in a temp_folder
    temp_files <- replicate(n = length(files_url),
                            expr = tempfile(fileext = file_ext))

    # loop to avoid rmd error when examples are generated
    for (i in 1:length(files_url)){

      download.file(files_url[i], destfile = temp_files[i], mode = "wb", quiet = TRUE)
    }

    # Check the downloaded file before loading
    if (any(file.info(temp_files)$size == 0)) {

      stop(red("😵 Any of downloaded files is empty. Please, be sure that you are connected to the internet."))

    }

    # Import the data
    candidacies_raw_info <-
      temp_files |>
      map(function(x) { get(load(x)) }) |>
      list_rbind()

    # include candidacies info to candidacies ballots data
    candidacies_data <-
      candidacies_data |>
      left_join(candidacies_raw_info,
                by = c("cod_elec", "type_elec",
                       "date_elec", "id_candidacies")) |>
      relocate(abbrev_candidacies:cod_candidacies_nat,
               .after = "id_candidacies") |>
      # same as id_candidacies
      # remove MIR codes
      select(-cod_candidacies_prov, -contains("MIR")) |>
      rename(id_candidacies_ccaa = cod_candidacies_ccaa,
             id_candidacies_nat = cod_candidacies_nat)

    if (short_version) {

      if (verbose) {

        message(yellow("⚠️ A short version was asked. If you require all variables, please run with `short_version = FALSE'"))

      }

      # Select just few variables
      candidacies_data <-
        candidacies_data |>
        select(id_elec, type_elec, date_elec, id_INE_poll_station,
               id_INE_mun, ccaa, prov, mun,
               id_candidacies, abbrev_candidacies, name_candidacies,
               ballots)
    }
    return(candidacies_data)
  }












