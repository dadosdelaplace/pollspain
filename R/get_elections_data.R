#' @title Get Municipal Census Data
#'
#' @description Fetch municipal census data for one or more elections at the municipal
#' level. This function downloads and processes raw municipal data files for the specified elections.
#'
#' @inheritParams type_to_code_election
#' @param type_elec A vector or single value representing the types of elections.
#' @param year A vector or single value representing the years of the elections to be considered.
#' @param month A vector or single value representing the months of the elections to be considered.
#' Ensure (refer to \code{dates_elections_spain}) that elections of the
#' specified type are available for the provided year and month.
#'
#' @return A tibble with rows corresponding to municipalities for each election, including the
#' following columns:
#' \item{cod_elec}{Code representing the type of election: \code{"01"} (referendum),
#' \code{"02"} (congress), \code{"03"} (senate), \code{"04"} (local elections),
#' \code{"06"} (cabildo - Canarian council - elections), \code{"07"}
#' (European Parliament elections).}
#' \item{type_elec}{Type of election.}
#' \item{date_elec}{Date of the election.}
#' \item{id_INE_mun}{Municipality ID constructed from the ccaa-prov-mun codes provided
#' by INE.}
#' \item{id_MIR_mun}{Municipality ID constructed from the ccaa-prov-mun codes provided
#' by the Spanish Ministry of Interior (MIR).}
#' \item{cod_INE_ccaa, cod_MIR_ccaa, ccaa}{Codes and names for regions (ccaa)
#' to which the municipalities belong.}
#' \item{cod_INE_prov, prov}{Codes and names for the provinces to which the municipalities belong.}
#' \item{cod_INE_mun, mun}{Code, and name for
#' municipalities.}
#' \item{cod_mun_jud_district, cod_mun_prov_council}{Codes for the judicial
#' district and provincial council.}
#' \item{n_poll_stations}{Number of polling stations in each municipality.}
#' \item{pop_res_mun}{Population census of residents (CER + CERA).}
#' \item{census_INE_mun}{Population from \code{pop_res_mun} eligible to vote.}
#' \item{census_counting_mun}{Eligible voters after claims.}
#' \item{census_CERE_mun}{Census of foreign nationals, relevant only for EU elections.}
#'
#' @details
#' This function fetches municipal-level data for the specified elections by downloading
#' the corresponding `.rda` files from GitHub and processing them into a tidy format.
#' It automatically handles the download, loading, and merging of data across multiple
#' election periods as specified by the user.
#'
#' @authors
#' Mikaela DeSmedt, Javier Álvarez-Liébana.
#' @source Some definitions of variables were extracted from
#' \url{https://www.ige.gal}.
#' @keywords get_elections_data
#' @name get_mun_census_data
#' @import crayon
#' @examples
#'
#' ## Correct examples ----
#'
#' # Congress elections in April 2019
#' # Fetch municipal census data for the congress elections in April 2019
#' mun_census_data1 <- get_mun_census_data("congress", 2019, 4)
#'
#' # Senate elections in April 2019
#' # Fetch municipal census data for the senate elections in April 2019
#' mun_census_data2 <- get_mun_census_data("senate", 2019, 4)
#'
#' # Example usage to combine data from different elections into one table
#' # Fetch municipal census data for congress elections in Nov 2019, April 2019, and June 2016
#' combined_mun_census_data <- get_mun_census_data(
#'   c("congress", "congress", "congress"),
#'   c(2019, 2019, 2016),
#'   c(11, 4, 6)
#' )
#'
#' ## Incorrect examples ----
#'
#' # Invalid election type
#' # This will fail because "national" is not a valid election type
#' mun_census_data5 <- get_mun_census_data("national", 2019, 4)
#'
#' # Length mismatch between year and month vectors
#' # This will fail because the length of the year and month vectors do not match
#' mun_census_data6 <- get_mun_census_data("congress", 2016, c(4, 11))
#'
#' # Invalid date format
#' # This will fail because the date should be split into year and month
#' mun_census_data7 <- get_mun_census_data("congress", "2016-06-26")
#'
#' # Non-existent election data
#' # This will fail because 1990 congress elections are not available
#' mun_census_data8 <- get_mun_census_data("congress", 1990, 4)
#'
#' @export


get_mun_census_data <- function(type_elec, year, month) {
  # Ensure input parameters are vectors
  type_elec_vec <- as.vector(type_elec)
  year_vec <- as.vector(year)
  month_vec <- as.vector(month)

  # Initialize an empty list to store the data
  combined_data_list <- list()

  # Loop through the vectors and fetch the data
  for (i in seq_along(type_elec_vec)) {
    type_elec <- type_elec_vec[i]
    year <- year_vec[i]
    month <- month_vec[i]

    # Ensure the month is two digits
    char_month <- sprintf("%02d", month)

    # Construct the URL for the specific election directory
    base_url <- "https://github.com/mikadsr/Pollspain-data/blob/main"
    election_info <- type_to_code_election(type_elec)
    election_dir <- glue("{base_url}/{election_info$dir}/{election_info$cod_elec}{year}{char_month}")

    # Construct the URL for the raw data file
    file_url <- glue("{election_dir}/raw_mun_data_{type_elec}_{year}_{char_month}.rda?raw=true")

    # Print the file URL for debugging purposes
    message("Fetching data from ", file_url)

    # Fetch the file using download.file instead of GET
    tryCatch({
      temp_file <- tempfile(fileext = ".rda")
      download.file(file_url, destfile = temp_file, mode = "wb")

      # Check the downloaded file before loading
      if (file.info(temp_file)$size == 0) {
        stop("Downloaded file is empty. The URL might be incorrect or the file may not exist.")
      }

      # Load the data into a new environment
      temp_env <- new.env()
      load(temp_file, envir = temp_env)
      df_name <- ls(temp_env)
      mun_data <- get(df_name, envir = temp_env)


      # Join MIR and INE information
      mun_data <- mun_data %>%
        left_join(cod_INE_mun, by = c("id_MIR_mun", "cod_MIR_ccaa", "cod_INE_prov", "cod_INE_mun"), suffix = c(".x", "")) %>%
        select(-mun.x) %>%
        relocate(cod_INE_ccaa, .before = cod_MIR_ccaa) %>%
        relocate(id_INE_mun, .before = id_MIR_mun) %>%
        relocate(mun, .after = cod_INE_mun) %>%
        relocate(ccaa, .after = cod_MIR_ccaa) %>%
        relocate(prov, .after = cod_INE_prov)

      # Add the data to the list
      combined_data_list[[i]] <- mun_data
    }, error = function(e) {
      message("Failed to fetch the data file for ", type_elec, " in ", year, "-", char_month, ": ", e$message)
    })
  }

  # Combine all data frames into one
  combined_mun_census_data <- bind_rows(combined_data_list)

  return(combined_mun_census_data)
}


#' @title Get Poll Station Data
#'
#' @description Fetch and process poll station data for given election types, years, and months.
#' This function supports both single values and vector inputs for fetching and combining data for multiple elections at once.
#'
#' @inheritParams type_to_code_election
#' @param year,month Vectors of years and months of elections to be considered.
#' Ensure (see \code{dates_elections_spain}) that elections of the provided type are available for the given year and month.
#' @param prec_round Number of decimal places to round percentage values to (default is 3).
#'
#' @return A tibble with poll station data including the following elements:
#' \item{id_elec}{ID of the election, constructed from the election code and date.}
#' \item{type_elec}{Type of election.}
#' \item{date_elec}{Date of election.}
#' \item{id_INE_poll_station}{ID of the poll station, constructed from the municipality and poll station codes.}
#' \item{ccaa,prov,mun}{Names and codes for regions, provinces, and municipalities.}
#' \item{census_counting}{Census count.}
#' \item{ballots_1,turnout_1}{Number of ballots and turnout percentage in the first round.}
#' \item{ballots_2,turnout_2}{Number of ballots and turnout percentage in the second round (if applicable).}
#' \item{blank_ballots,invalid_ballots,party_ballots,valid_ballots,total_ballots}{Various ballot counts.}
#' \item{turnout,porc_valid,porc_invalid,porc_parties,porc_blank}{Various turnout and percentage metrics.}
#' \item{pop_res_mun}{Population residing in the municipality.}
#'
#' @details
#' This function fetches poll station-level data for the specified elections by downloading
#' the corresponding `.rda` files from GitHub and processing them into a tidy format.
#' It automatically handles the download, loading, and merging of data across multiple
#' election periods as specified by the user.
#'
#' @authors
#' Mikaela DeSmedt, Javier Álvarez-Liébana
#'
#' @examples
#'
#' ## Correct examples
#' # Fetching data for single elections
#' poll_station_data1 <- get_poll_station_data("congress", 2019, 4)
#' poll_station_data2 <- get_poll_station_data("senate", 2019, 4)
#'
#' # Fetching data for multiple elections at once
#' combined_poll_station_data <- get_poll_station_data(
#'   c("congress", "congress", "congress"),
#'   c(2019, 2019, 2016),
#'   c(11, 4, 6)
#' )
#'
#' ## Incorrect examples
#' # Invalid election type
#' poll_station_data5 <- get_poll_station_data("national", 2019, 4)
#'
#' # Length mismatch between year and month vectors
#' poll_station_data6 <- get_poll_station_data("congress", 2016, c(4, 11))
#'
#' # Invalid date format
#' poll_station_data7 <- get_poll_station_data("congress", "2016-06-26")
#'
#' # Non-existent election data
#' poll_station_data8 <- get_poll_station_data("congress", 1990, 4)
#'
#'
#' @export


get_poll_station_data <- function(type_elec, year, month, prec_round = 3) {
  # Ensure the inputs are vectors of the same length
  if (!is.vector(type_elec) | !is.vector(year) | !is.vector(month)) {
    stop("All inputs must be vectors.")
  }
  if (length(type_elec) != length(year) | length(year) != length(month)) {
    stop("All input vectors must be of the same length.")
  }

  # Initialize an empty list to store the data
  combined_data_list <- list()

  # Local helper function to read RDA file from GitHub
  read_rda_from_github <- function(url) {
    temp_file <- tempfile(fileext = ".rda")
    GET(url, write_disk(temp_file, overwrite = TRUE))
    temp_env <- new.env()
    load(temp_file, envir = temp_env)
    df_name <- ls(temp_env)
    data <- get(df_name, envir = temp_env)
    return(data)
  }

  # Loop through the vectors and fetch the data
  for (i in seq_along(type_elec)) {
    # Ensure the month is two digits
    char_month <- sprintf("%02d", month[i])

    # Check if the provided election type is valid
    election_info <- type_to_code_election(type_elec[i])

    # Base URL for data
    base_url <- "https://github.com/mikadsr/Pollspain-data/blob/main"

    # Construct the URL for the specific election directory
    election_dir <- glue("{base_url}/{election_info$dir}/{election_info$cod_elec}{year[i]}{char_month}")
    message("Fetching from URL: ", election_dir)

    # Construct the URL for the specific file
    data_url <- glue("{election_dir}/raw_poll_stations_{type_elec[i]}_{year[i]}_{char_month}.rda?raw=true")
    message("Fetching data from ", data_url)

    # Fetch the data
    tryCatch({
      poll_station_data <- read_rda_from_github(data_url)

      # Process the data
      poll_station_data <- poll_station_data %>%
        mutate(valid_ballots = blank_ballots + party_ballots,
               total_ballots = valid_ballots + invalid_ballots) %>%
        filter(cod_INE_mun != "999") %>%
        left_join(get_mun_census_data(type_elec[i], year[i], month[i]),
                  by = c("cod_elec", "type_elec", "date_elec", "id_MIR_mun"),
                  suffix = c("", ".y")) %>%
        select(-contains(".y")) %>%
        relocate(id_INE_mun, .before = id_MIR_mun) %>%
        relocate(cod_INE_ccaa, .before = cod_MIR_ccaa) %>%
        relocate(ccaa, .after = cod_MIR_ccaa) %>%
        relocate(prov, .after = cod_INE_prov) %>%
        relocate(mun, .after = cod_INE_mun) %>%
        select(-c(census_counting_mun, census_CERE_mun, census_INE_mun)) %>%
        bind_rows(poll_station_data %>% filter(cod_INE_mun == "999")) %>%
        left_join(cod_INE_mun %>%
                    distinct(cod_MIR_ccaa, cod_INE_prov, .keep_all = TRUE) %>%
                    select(contains("ccaa") | contains("prov")),
                  by = c("cod_MIR_ccaa", "cod_INE_prov"),
                  suffix = c("", ".y")) %>%
        mutate(cod_INE_ccaa = ifelse(is.na(cod_INE_ccaa), cod_INE_ccaa.y, cod_INE_ccaa),
               ccaa = ifelse(is.na(ccaa), ccaa.y, ccaa),
               prov = ifelse(is.na(prov), prov.y, prov),
               mun = ifelse(cod_INE_mun == "999", "CERA", mun),
               id_INE_mun = glue("{cod_INE_ccaa}-{cod_INE_prov}-{cod_INE_mun}"),
               pop_res_mun = ifelse(cod_INE_mun == "999", census_INE, pop_res_mun)) %>%
        select(-contains(".y"), -cod_MIR_ccaa) %>%
        filter(!is.na(id_INE_mun)) %>%
        mutate(id_INE_poll_station = glue("{id_INE_mun}-{cod_mun_district}-{cod_sec}-{cod_poll_station}"),
               turnout_1 = round(100 * ballots_1 / census_counting, prec_round),
               turnout_2 = round(100 * ballots_2 / census_counting, prec_round),
               turnout = round(100 * total_ballots / census_counting, prec_round),
               turnout_abs = 100 - turnout,
               porc_valid = round(100 * valid_ballots / total_ballots, prec_round),
               porc_invalid = round(100 * invalid_ballots / total_ballots, prec_round),
               porc_parties = round(100 * party_ballots / valid_ballots, prec_round),
               porc_blank = round(100 * blank_ballots / valid_ballots, prec_round)) %>%
        relocate(turnout:porc_blank, .after = total_ballots) %>%
        relocate(id_INE_poll_station, .after = date_elec) %>%
        relocate(turnout_1, .after = ballots_1) %>%
        relocate(turnout_2, .after = ballots_2) %>%
        mutate(id_elec = glue("{election_info$cod_elec}-{date_elec}")) %>%
        select(id_elec, type_elec, date_elec, id_INE_poll_station, ccaa, prov, mun,
               census_counting, ballots_1, turnout_1, ballots_2, turnout_2,
               blank_ballots, invalid_ballots, party_ballots, valid_ballots,
               total_ballots, turnout, porc_valid, porc_invalid,
               porc_parties, porc_blank, pop_res_mun)

      # Add the processed data to the list
      combined_data_list[[i]] <- poll_station_data
    }, error = function(e) {
      message(glue("No data found for election {type_elec[i]} in {char_month}-{year[i]}: {e$message}"))
    })
  }

  # Combine all data frames into one
  combined_poll_station_data <- bind_rows(combined_data_list)

  return(combined_poll_station_data)
}

#' @title Get Candidates Data
#'
#' @description
#' Fetch and process candidates data for Spanish elections. This function retrieves and processes candidate data specifically for congress elections in Spain. It fetches raw data from the Pollspain Data Repository, processes it, and returns a detailed data frame containing information about candidates in the specified election(s).
#'
#' @inheritParams get_mun_census_data
#'
#' @param election_type Type of election to retrieve data for. Must be one of the following: "congress".
#' @param year Year of the election.
#' @param month Month of the election.
#'
#' @return
#' A tibble containing the candidates data with the following columns:
#' \item{cod_elec}{Code representing the type of election: \code{"01"} (referendum), \code{"02"} (congress), \code{"03"} (senate), \code{"04"} (local elections), \code{"06"} (cabildo - Canarian council - elections), \code{"07"} (European Parliament elections).}
#' \item{type_elec}{Type of election.}
#' \item{date_elec}{Date of the election.}
#' \item{id_INE_mun}{Municipality ID constructed from the ccaa-prov-mun codes provided by INE.}
#' \item{id_MIR_mun}{Municipality ID constructed from the ccaa-prov-mun codes provided by the Spanish Ministry of Interior (MIR).}
#' \item{cod_INE_ccaa, cod_MIR_ccaa, ccaa}{Codes and names for regions (ccaa) to which the municipalities belong.}
#' \item{cod_INE_prov, prov}{Codes and names for the provinces to which the municipalities belong.}
#' \item{cod_INE_mun, mun}{Code, and name for municipalities.}
#' \item{candidacies}{Data related to the individual candidacies, including party affiliation and candidate information.}
#'
#' @details
#' This function retrieves candidate data specifically for congress elections in Spain by accessing the Pollspain Data Repository. It processes and formats the raw data into a tibble that includes detailed information about each candidate and their candidacy.
#'
#' @authors
#' Mikaela DeSmedt, Javier Álvarez-Liébana
#'
#' @examples
#'
#' ## Correct examples
#' # Get candidates data for congress elections in March 1996
#' candidates_data_1996_03 <- get_candidates_data("congress", 1996, 3)
#' print(candidates_data_1996_03)
#'
#' # Get candidates data for congress elections in April 2019
#' candidates_data_2019_04 <- get_candidates_data("congress", 2019, 4)
#' print(candidates_data_2019_04)
#'
#' ## Incorrect examples
#' # Trying to get candidates data for non-congress elections
#' try(get_candidates_data("senate", 1996, 3))
#'
#' # Trying to get candidates data for non-existent elections
#' try(get_candidates_data("congress", 1800, 1))
#'
#' @source Data extracted from \href{https://github.com/mikadsr/Pollspain-data}{Pollspain Data Repository}
#' @keywords get_elections_data
#' @name get_candidates_data
#' @export
get_candidates_data <- function(type_elec, year, month) {
  # At this time, just congress election
  if (type_elec != "congress") {
    stop("Development in process: at this time, just congress elections are allowed")
  }

  # Check if elections required are allowed
  elections_allowed <- dates_elections_spain %>%
    filter(year >= 1986) %>%
    inner_join(tibble(cod_elec = type_to_code_election(type_elec)$cod_elec,
                      type_elec, year, month),
               by = c("cod_elec", "type_elec", "year", "month"))
  join_result <- elections_allowed %>% nrow()
  if (join_result == 0) {
    stop("No elections on provided dates are available")
  }

  # Construct the URL for the data file
  election_info <- type_to_code_election(type_elec)
  base_url <- "https://github.com/mikadsr/Pollspain-data/blob/main"
  file_url <- glue("{base_url}/{election_info$dir}/{election_info$cod_elec}{year}{sprintf('%02d', month)}/raw_candidates_{type_elec}_{year}_{sprintf('%02d', month)}.rda?raw=true")

  # Print the URL for debugging purposes
  message("Fetching data from URL: ", file_url)

  # Fetch and load the .rda file
  candidates_data <- tryCatch({
    temp_file <- tempfile(fileext = ".rda")
    GET(file_url, write_disk(temp_file, overwrite = TRUE))
    temp_env <- new.env()
    load(temp_file, envir = temp_env)
    df_name <- ls(temp_env)
    data <- get(df_name, envir = temp_env)
    data
  }, error = function(e) {
    stop("Failed to fetch or read candidates data from the specified URL.")
  })


  # Convert date_elec to Date format (YYYY-MM-DD)
  candidates_data <- candidates_data %>%
    mutate(date_elec = ymd(date_elec))   # Ensure date_elec is in Date format


  # Output
  return(candidates_data)
}

#' @title Get Candidacies Data
#'
#' @description
#' Fetch and process candidacies data for Spanish elections. This function supports fetching data for multiple elections simultaneously by accepting vector inputs for the election parameters.
#'
#' @param type_elec Character vector of election types. Valid types are: 'referendum', 'congress', 'senate', 'local', 'regional', 'cabildo', 'EU'.
#' @param year Integer vector of election years.
#' @param month Integer vector of election months.
#' @param include_candidates Logical flag indicating whether to include detailed candidates data. Default is FALSE.
#'
#' @return
#' A tibble containing the candidacies data.
#'
#' @details
#' This function retrieves and processes candidacies data for Spanish elections by downloading and merging data for the specified election types, years, and months. If `include_candidates` is set to TRUE, the function will also include detailed information about each candidate.
#'
#' @authors
#' Mikaela DeSmedt, Javier Álvarez-Liébana
#'
#' @examples
#'
#' ## Correct examples
#' # Single election
#' candidates_data <- get_candidacies_data("congress", 1986, 6)
#'
#' # Multiple elections
#' combined_candidates_data <- get_candidacies_data(
#'   c("congress", "congress", "senate"),
#'   c(2019, 2016, 2016),
#'   c(11, 6, 6)
#' )
#'
#' # With candidates details
#' candidates_data_with_details <- get_candidacies_data(
#'   "congress",
#'   2019,
#'   4,
#'   include_candidates = TRUE
#' )
#'
#' ## Incorrect examples
#' # Invalid election type
#' candidates_data_invalid <- get_candidacies_data("national", 2019, 4)
#'
#' # Length mismatch between year and month
#' candidates_data_mismatch <- get_candidacies_data("congress", 2016, c(4, 11))
#'
#' # Invalid date format
#' candidates_data_invalid_date <- get_candidacies_data("congress", "2016-06-26")
#'
#' # Non-existent election data
#' candidates_data_non_existent <- get_candidacies_data("congress", 1990, 4)
#'
#' @source Data extracted from \href{https://github.com/mikadsr/Pollspain-data}{Pollspain Data Repository}
#' @keywords get_elections_data
#' @export

get_candidacies_data <- function(type_elec, year, month, include_candidates = FALSE) {

  # Ensure type_elec is a character vector
  type_elec <- as.character(type_elec)

  # Initialize an empty list to store the data
  combined_data_list <- list()

  # Loop through each set of parameters
  for (i in seq_along(type_elec)) {
    # Validate election type
    if (!type_elec[i] %in% c("congress", "senate")) {
      stop("Invalid election type. Allowed types are: 'congress', 'senate'")
    }

    # Get election info from the utils.R script
    election_info <- type_to_code_election(type_elec[i])

    # Construct the base URL and directory URL
    base_url <- "https://github.com/mikadsr/Pollspain-data/blob/main"
    election_dir <- glue("{base_url}/{election_info$dir}/{election_info$cod_elec}{year[i]}{sprintf('%02d', month[i])}")

    # Print the final URL for debugging
    message("Fetching from URL: ", election_dir)

    # Fetch the candidacies data
    data_url <- glue("{election_dir}/raw_candidacies_{type_elec[i]}_{year[i]}_{sprintf('%02d', month[i])}.rda?raw=true")
    message("Fetching data from ", basename(data_url))

    # Collect raw data
    candidacies_files <- tryCatch({
      temp_file <- tempfile(fileext = ".rda")
      GET(data_url, write_disk(temp_file, overwrite = TRUE))
      temp_env <- new.env()
      load(temp_file, envir = temp_env)
      df_name <- ls(temp_env)
      data <- get(df_name, envir = temp_env)
      data
    }, error = function(e) {
      message(glue("No data found for election {type_elec[i]} in {sprintf('%02d', month[i])}-{year[i]}"))
      return(NULL)
    })

    if (is.null(candidacies_files)) next

    # Include candidates if requested
    if (include_candidates) {
      # Fetch the candidates data
      candidates_url <- glue("{election_dir}/raw_candidates_{type_elec[i]}_{year[i]}_{sprintf('%02d', month[i])}.rda?raw=true")
      message("Fetching candidates data from ", basename(candidates_url))

      candidates_files <- tryCatch({
        temp_file <- tempfile(fileext = ".rda")
        GET(candidates_url, write_disk(temp_file, overwrite = TRUE))
        temp_env <- new.env()
        load(temp_file, envir = temp_env)
        df_name <- ls(temp_env)
        data <- get(df_name, envir = temp_env)
        data
      }, error = function(e) {
        message(glue("No candidates data found for election {type_elec[i]} in {sprintf('%02d', month[i])}-{year[i]}"))
        return(NULL)
      })

      if (is.null(candidates_files)) next

      # Join candidacies and candidates data, keeping only the x variables in case of duplication
      candidacies_data <- candidacies_files %>%
        left_join(candidates_files, by = "id_candidacies", suffix = c("", ".y")) %>%
        select(-ends_with(".y")) %>%
        rename(candidate_full_name = candidate_full_name,
               candidate_order = order_number,
               candidate_type = candidate_type,
               candidate_sex = candidate_sex)

    } else {
      candidacies_data <- candidacies_files
    }

    # Add the processed data to the list
    combined_data_list[[i]] <- candidacies_data
  }

  # Combine all data frames into one
  combined_candidacies_data <- bind_rows(combined_data_list)

  return(combined_candidacies_data)
}

#' @title Get CERA Data (at Poll Station Level)
#'
#' @description
#' This function aggregates CERA (Census of Absent Residents) data from election data based on a specified hierarchical level. The function processes election data to provide aggregated statistics such as census counts, total ballots, and turnout percentages.
#'
#' @inheritParams get_mun_census_data
#' @param election_data A data frame containing the election data to be processed.
#' @param id_col The name of the column containing the poll station ID. Defaults to \code{"id_INE_poll_station"}.
#' @param level The hierarchical level for data aggregation. Can be one of \code{"all"}, \code{"ccaa"}, \code{"prov"}, \code{"mun"}, \code{"mun_district"}, \code{"sec"}, or \code{"poll_station"}. Defaults to \code{"all"}.
#' @param cod_CERA The code representing CERA. Defaults to \code{"999"}.
#' @param prec_round The precision for rounding percentages. Defaults to \code{3}.
#'
#' @return
#' A data frame with the aggregated CERA data, including columns for type of election, election date, census data, total ballots, and turnout percentages.
#'
#' @details
#' This function processes election data to aggregate CERA statistics based on the specified hierarchical level. It supports various aggregation levels and handles rounding of percentage values based on the provided precision.
#'
#' @authors
#' Mikaela DeSmedt (documentation), Javier Álvarez-Liébana
#'
#' @examples
#'
#' ## Correct Examples
#'
#' # Fetch CERA data aggregated at the municipal level
#' cera_data_mun <- get_CERA_data(
#'   poll_station_data1,
#'   id_col = "id_INE_poll_station",
#'   level = "mun"
#' )
#' print(cera_data_mun)
#'
#' # Fetch CERA data aggregated at the provincial level
#' cera_data_prov <- get_CERA_data(
#'   poll_station_data1,
#'   id_col = "id_INE_poll_station",
#'   level = "prov"
#' )
#' print(cera_data_prov)
#'
#' ## Incorrect Examples
#'
#' # Attempt to fetch CERA data with a non-existent id column, should raise an error
#' \dontrun{
#' cera_data_invalid_col <- get_CERA_data(
#'   poll_station_data1,
#'   id_col = "non_existent_column",
#'   level = "mun"
#' )
#' }
#'
#' # Attempt to fetch CERA data with an invalid aggregation level, should raise an error
#' \dontrun{
#' cera_data_invalid_level <- get_CERA_data(
#'   poll_station_data1,
#'   id_col = "id_INE_poll_station",
#'   level = "barrio"
#' )
#' }
#'
#' @source Data extracted and processed from various election sources.
#' @keywords get_elections_data
#' @name get_CERA_data
#' @export
get_CERA_data <- function(election_data, id_col = "id_INE_poll_station",
                          level = "all", cod_CERA = "999", prec_round = 3) {

  # Aggregation of data
  hierarchy_levels <- c("ccaa", "prov", "mun", "mun_district", "sec", "poll_station")

  # Extract CERA data
  cera_data <- election_data %>%
    filter(grepl(cod_CERA, .data[[id_col]])) %>%
    mutate(cod_INE_ccaa = substr(id_INE_poll_station, 1, 2),
           cod_INE_prov = substr(id_INE_poll_station, 4, 5),
           cod_INE_mun = substr(id_INE_poll_station, 7, 9),
           cod_mun_district = substr(id_INE_poll_station, 11, 12),
           cod_sec = substr(id_INE_poll_station, 14, 15),
           cod_poll_station = substr(id_INE_poll_station, 17, 17))

  # Grouping
  if (level == "all") {
    group_vars <- "id_elec"
  } else {
    levels <- hierarchy_levels[1:which(hierarchy_levels == level)]
    group_vars <- c("id_elec", paste0("cod_INE_", levels))
  }

  # Summarize CERA data
  data_cera <- cera_data %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(type_elec = unique(type_elec),
              date_elec = unique(date_elec),
              census_cera = sum(census_counting, na.rm = TRUE),
              total_ballots_cera = sum(total_ballots, na.rm = TRUE),
              turnout_cera = round(100 * total_ballots_cera / census_cera, prec_round),
              .groups = 'drop')

  return(data_cera)
}

#' @title Get Candidacy Ballot Data
#'
#' @description
#' This function fetches and combines data on ballots cast for each candidacy in a specified election. It optionally includes the names of the candidacies. The data is retrieved from a specified GitHub repository containing Spanish election data.
#'
#' @param type_elec Character vector. Specifies the type of election, either "congress" or "senate".
#' @param year Integer vector. Specifies the year(s) of the election(s).
#' @param month Integer vector. Specifies the month(s) of the election(s). The month should be in two digits (e.g., 03 for March).
#' @param include_candidacy_names Logical. If TRUE, the function will include the names of the candidacies by fetching additional data. Default is TRUE.
#'
#' @return
#' A tibble containing the combined ballot data and, if requested, the names of the candidacies. The tibble includes the following columns (depending on the availability of data):
#' \item{cod_elec}{Election code.}
#' \item{type_elec}{Type of election.}
#' \item{date_elec}{Date of the election.}
#' \item{id_MIR_mun}{Unique ID for the municipality.}
#' \item{cod_MIR_ccaa}{Autonomous community code.}
#' \item{cod_INE_prov}{Province code.}
#' \item{cod_INE_mun}{Municipality code.}
#' \item{cod_mun_district}{Municipal district code.}
#' \item{cod_sec}{Section code.}
#' \item{cod_poll_station}{Polling station code.}
#' \item{turn}{Turnout.}
#' \item{id_candidacies}{Unique ID for the candidacies.}
#' \item{ballots}{Number of ballots cast.}
#' \item{abbrev_candidacies}{Abbreviation of the candidacies (if \code{include_candidacy_names} is TRUE).}
#' \item{name_candidacies}{Full name of the candidacies (if \code{include_candidacy_names} is TRUE).}
#'
#' @details
#' The function retrieves data from a specified GitHub repository containing raw data files for Spanish congressional and senatorial elections. The function merges the ballots data with the candidacy names data if the \code{include_candidacy_names} parameter is set to TRUE. It ensures that no duplicate columns are retained in the final output.
#'
#' @authors
#' Mikaela DeSmedt, Javier Álvarez-Liébana
#'
#' @examples
#' # Correct Use Cases
#'
#' # Fetch ballots data for the 2004 congressional election in March, including candidacy names
#' ballots_data <- get_candidacy_ballot_data("congress", 2004, 03)
#' print(ballots_data)
#'
#' # Fetch ballots data for the 2023 congressional election in July, excluding candidacy names
#' ballots_data <- get_candidacy_ballot_data("congress", 2023, 07, include_candidacy_names = FALSE)
#' print(ballots_data)
#'
#' # Incorrect Use Cases
#'
#' # Attempt to fetch ballots data with an invalid election type
#' \dontrun{
#' ballots_data <- get_candidacy_ballot_data("parliament", 2023, 07)
#' }
#' # This will raise an error because "parliament" is not a valid election type. The function only accepts "congress" or "senate".
#'
#' # Attempt to fetch ballots data with an invalid month format
#' \dontrun{
#' ballots_data <- get_candidacy_ballot_data("congress", 2023, 13)
#' }
#' # This will raise an error or produce unexpected results because "13" is not a valid month. Months should be in two-digit format (e.g., 01 for January).
#'
#' # Attempt to fetch ballots data with an invalid year format
#' \dontrun{
#' ballots_data <- get_candidacy_ballot_data("senate", 2023.5, 07)
#' }
#' # This will raise an error because the year should be an integer, not a decimal.
#'
#' @export
get_candidacy_ballot_data <- function(type_elec, year, month, include_candidacy_names = TRUE) {

  # Ensure type_elec is a character vector
  type_elec <- as.character(type_elec)

  # Initialize an empty list to store the data
  combined_data_list <- list()

  # Loop through each set of parameters
  for (i in seq_along(type_elec)) {
    # Validate election type
    if (!type_elec[i] %in% c("congress", "senate")) {
      stop("Invalid election type. Allowed types are: 'congress', 'senate'")
    }

    # Get election info from the utils.R script
    election_info <- type_to_code_election(type_elec[i])

    # Construct the base URL and directory URL
    base_url <- "https://github.com/mikadsr/Pollspain-data/blob/main"
    election_dir <- glue("{base_url}/{election_info$dir}/{election_info$cod_elec}{year[i]}{sprintf('%02d', month[i])}")

    # Print the final URL for debugging
    message("Fetching from URL: ", election_dir)

    # Fetch the ballots data
    ballots_url <- glue("{election_dir}/raw_candidacies_poll_{type_elec[i]}_{year[i]}_{sprintf('%02d', month[i])}.rda?raw=true")
    message("Fetching data from ", basename(ballots_url))

    # Collect raw data
    ballots_data <- tryCatch({
      temp_file <- tempfile(fileext = ".rda")
      GET(ballots_url, write_disk(temp_file, overwrite = TRUE))
      temp_env <- new.env()
      load(temp_file, envir = temp_env)
      df_name <- ls(temp_env)
      data <- get(df_name, envir = temp_env)
      data
    }, error = function(e) {
      message(glue("No ballots data found for election {type_elec[i]} in {sprintf('%02d', month[i])}-{year[i]}"))
      return(NULL)
    })

    if (is.null(ballots_data)) next

    # Optionally include the candidacy names
    if (include_candidacy_names) {
      candidacies_url <- glue("{election_dir}/raw_candidacies_{type_elec[i]}_{year[i]}_{sprintf('%02d', month[i])}.rda?raw=true")
      message("Fetching candidacy names data from ", basename(candidacies_url))

      candidacies_data <- tryCatch({
        temp_file <- tempfile(fileext = ".rda")
        GET(candidacies_url, write_disk(temp_file, overwrite = TRUE))
        temp_env <- new.env()
        load(temp_file, envir = temp_env)
        df_name <- ls(temp_env)
        data <- get(df_name, envir = temp_env)
        data
      }, error = function(e) {
        message(glue("No candidacy names data found for election {type_elec[i]} in {sprintf('%02d', month[i])}-{year[i]}"))
        return(NULL)
      })

      if (!is.null(candidacies_data)) {
        # Merge ballots data with candidacy names data, avoiding duplicate columns
        ballots_data <- ballots_data %>%
          left_join(candidacies_data, by = "id_candidacies") %>%
          select(-matches("\\.y$")) %>%  # Remove columns with suffix ".y" after the join
          rename_with(~ sub("\\.x$", "", .x))  # Remove ".x" suffix from columns
      }
    }

    # Add the processed data to the list
    combined_data_list[[i]] <- ballots_data
  }

  # Combine all data frames into one
  combined_ballots_data <- bind_rows(combined_data_list)

  return(combined_ballots_data)
}

#' @title Aggregate Election Data
#'
#' @description
#' This function aggregates election data at various hierarchical levels based on geographic scope and candidacy. It allows summarizing ballots by different levels such as Autonomous Communities (CCAA), Provinces, and Municipalities, while retaining key election information.
#'
#' @param ballots_data A data frame containing the election data, including ballots and candidacy information.
#' @param scope The geographic level for data aggregation. Can be one of \code{"ccaa"}, \code{"prov"}, or \code{"mun"}. Defaults to \code{"ccaa"}.
#' @param group_by_candidacy A logical value indicating whether to group data by candidacy name (\code{name_candidacies}). Defaults to \code{TRUE}.
#'
#' @return
#' A data frame with the aggregated election data, including the total ballots and relevant election identifiers (e.g., \code{cod_elec}, \code{type_elec}, \code{date_elec}, \code{id_candidacies}, \code{abbrev_candidacies}, and \code{name_candidacies} if applicable).
#'
#' @details
#' The function summarizes election data based on the specified geographic scope and optionally groups the data by candidacy name. It supports aggregation at different hierarchical levels and ensures that the aggregated data retains key election identifiers.
#'
#' @authors
#' Mikaela DeSmedt, Javier Álvarez-Liébana
#'
#' @source Data extracted and processed from various election sources.
#'
#' @keywords aggregate_election_data, election_data, ballots, geographic_scope, candidacy
#'
#' @examples
#'
#' ## Correct examples ----
#'
#' # Aggregate election data by province and candidacy
#' ballots_data <- data.frame(
#'   cod_elec = "02",
#'   type_elec = "congress",
#'   date_elec = as.Date("2023-07-23"),
#'   id_MIR_mun = "01-04-001",
#'   cod_MIR_ccaa = "01",
#'   cod_INE_prov = "04",
#'   cod_INE_mun = "001",
#'   id_candidacies = "000053",
#'   abbrev_candidacies = "PSOE",
#'   name_candidacies = "Partido Socialista Obrero Español",
#'   ballots = 500
#' )
#' aggregated_data <- aggregate_election_data(ballots_data, scope = "prov")
#' print(aggregated_data)
#'
#' # Aggregate election data by Autonomous Community without candidacy names
#' aggregated_data_ccaa <- aggregate_election_data(ballots_data, scope = "ccaa", group_by_candidacy = FALSE)
#' print(aggregated_data_ccaa)
#'
#' ## Incorrect examples ----
#'
#' # Attempt to aggregate data with an invalid scope argument
#' \dontrun{
#' aggregated_data_invalid <- aggregate_election_data(ballots_data, scope = "region")
#' }
#' # This will raise an error because "region" is not a valid scope.
#'
#' # Attempt to aggregate data with missing required columns
#' incomplete_ballots_data <- data.frame(
#'   cod_elec = "02",
#'   type_elec = "congress",
#'   date_elec = as.Date("2023-07-23"),
#'   ballots = 500
#' )
#' \dontrun{
#' aggregated_data_missing <- aggregate_election_data(incomplete_ballots_data, scope = "mun")
#' }
#' # This will fail or produce incorrect results due to missing required columns.
#'
#' @export
aggregate_election_data <- function(ballots_data,
                                    scope = c("ccaa", "prov", "mun"),
                                    group_by_candidacy = TRUE) {
  # Ensure valid scope input
  scope <- match.arg(scope)

  # Extract year from date_elec if it's not already in the data
  ballots_data <- ballots_data %>%
    mutate(year = year(date_elec))

  # Determine grouping variables based on the scope
  group_vars <- c("year", "cod_elec", "type_elec", "date_elec",
                  "id_candidacies", "abbrev_candidacies")

  if (group_by_candidacy) {
    group_vars <- c(group_vars, "name_candidacies")
  }

  if (scope == "ccaa") {
    group_vars <- c(group_vars, "cod_MIR_ccaa")
  } else if (scope == "prov") {
    group_vars <- c(group_vars, "cod_MIR_ccaa", "cod_INE_prov")
  } else if (scope == "mun") {
    group_vars <- c(group_vars, "cod_MIR_ccaa", "cod_INE_prov", "cod_INE_mun")
  }

  # Aggregate data by the determined grouping variables
  aggregated_data <- ballots_data %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(total_ballots = sum(ballots, na.rm = TRUE), .groups = "drop")

  return(aggregated_data)
}
