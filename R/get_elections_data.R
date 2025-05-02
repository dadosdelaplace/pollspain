
#' @title Function to get jointly information on polling
#' stations and votes for candidacies.
#'
#' @description Import and preprocess poll stations info but jointly
#' with the preprocessed candidacies data, for given election types
#' and dates, at poll station level. This function supports both
#' single values and vector inputs for fetching and combining data
#' for multiple elections at once.
#'
#' @inheritParams import_poll_station_data
#' @inheritParams import_candidacies_data
#' @param election_data A database containing general election data
#' already provided (by other functions or by the user). Database
#' should contain a \code{col_id_elec} column.
#' Defaults to \code{NULL}.
#' @param ballots_data A database containing ballots data for
#' the different candidacies, already provided (by other functions or
#' by the user). Database should contain a
#' \code{col_id_poll_station} column. Defaults to \code{NULL}.
#' @param col_id_elec Column name in \code{election_data} to uniquely
#' identify the elections. Defaults to \code{"id_elec"}.
#' @param col_id_poll_station Column name in \code{ballots_data} to
#' uniquely identify poll stations. Defaults to
#' \code{"id_INE_poll_station"}.
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
#' \item{cod_INE_prov, prov}{codes and names for the provinces to which
#' the municipalities belong. Codes available only for long version.}
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
#' \item{n_poll_stations}{number of polling stations.}
#' \item{pop_res_mun}{population census of residents (CER + CERA) at
#' municipality level.}
#' \item{census_counting_mun}{population eligible to vote after
#' claims at municipality level.}
#' \item{id_candidacies}{id for candidacies (at province level).}
#' \item{id_candidacies_ccaa, id_candidacies_nat}{id for
#' candidacies (at region - ccaa - and national level).}
#' \item{abbrev_candidacies, name_candidacies}{acronym and full name
#' of the candidacies.}
#' \item{ballots}{number of ballots obtained for each candidacy at
#' each poll station.}
#'
#' @details This function fetches municipal-level data for the
#' specified elections by downloading the corresponding `.rda` files
#' from GitHub \url{https://github.com/dadosdelaplace/pollspain-data}
#' (directly downloaded from MIR website) and processing them into a
#' tidy format. It automatically handles the download, loading, and
#' merging of data across multiple election periods as specified by
#' the user.
#'
#' @author Javier Álvarez-Liébana and David Pereiro-Pol.
#' @keywords get_elections_data
#' @name get_election_data
#' @import crayon
#' @examples
#'
#' ## Correct examples
#'
#' # Congress elections in April 2019
#' # Fetch elections data for the congress elections in April 2019
#' elections_data1 <- get_election_data("congress", 2019, 4)
#'
#' # Fetch elections data for the congress and senate elections
#' # in April 2019 in a long version
#' elections_data2 <-
#'   get_election_data(c("congress", "senate"), 2019, 4,
#'                     short_version = FALSE)
#'
#' # Fetch elections data for congress elections in Nov 2019,
#' # April 2019, and June 2016
#' combined_elections_data <-
#'   get_election_data(c("congress", "congress"),
#'                     c(2019, 2016), c(11, 6))
#'
#' # Example usage providing date instead of month and year
#' date_example <- get_election_data("congress", date = "2016-06-26")
#'
#' # Example usage providing external tables
#' election_data <- import_poll_station_data("congress", 2019, 4)
#' ballots_data <- import_candidacies_data("congress", 2019, 4)
#' join_data <-
#'   get_election_data("congress", 2019, 4,
#'                      election_data = election_data,
#'                      ballots_data = ballots_data)
#'
#' # Example usage providing external tables with different
#' # column names
#' election_data <-
#'   import_poll_station_data("congress", 2019, 4) |>
#'   dplyr::rename(invent_1 = id_elec, invent_2 = id_INE_poll_station)
#' ballots_data <-
#'   import_candidacies_data("congress", 2019, 4) |>
#'   dplyr::rename(invent_1 = id_elec, invent_2 = id_INE_poll_station)
#' join_data <-
#'   get_election_data("congress", 2019, 4,
#'                     election_data = election_data,
#'                     ballots_data = ballots_data,
#'                     col_id_elec = "invent_1",
#'                     col_id_poll_station = "invent_2")
#'
#' # ----
#' # Incorrect examples
#' # ----
#'
#' \dontrun{
#' # Wrong examples
#'
#' # Invalid election type: "national" is not a valid election type
#' get_election_data("national", 2019, 4)
#'
#' # Invalid election: no congress elections are available in 5-2019.
#' # Please check dataset dates_elections_spain
#' get_election_data("congress", 2019, 5)
#'
#' # Invalid date format: date should be in %Y-%m-%d format
#' get_election_data("congress", date = "26-06-2016")
#'
#' # Invalid short version flag: short_version should be a
#' # logical variable
#' get_election_data("congress", 2019, 4, short_version = "yes")
#'
#' # Invalid include_candidates flag: include_candidates should be
#' # a logical variable
#' get_election_data("congress", 2019, 4, include_candidates = "no")
#'
#' # Invalid key columns: col_id_elec and col_id_poll should be
#' # columns included in datasets election_data and ballots_data
#' election_data <- import_poll_station_data("congress", 2019, 4)
#' ballots_data <- import_candidacies_data("congress", 2019, 4)
#' get_election_data("congress", 2019, 4,
#'                    election_data = election_data,
#'                    ballots_data = ballots_data,
#'                    col_id_elec = "invent_1",
#'                    col_id_poll_station = "invent_2")
#' }
#'
#' @export
get_election_data <-
  function(type_elec, year = 2019, date = NULL,
           election_data = NULL, ballots_data = NULL,
           col_id_elec = "id_elec", col_id_poll_station = "id_INE_poll_station",
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

    if (is.null(election_data)) {

      if (verbose) {

        # Print the file URL for debugging purposes
        message(blue("📦 Import poll station data ..."))
        Sys.sleep(1/20)

      }

      election_data <-
        import_poll_station_data(type_elec = type_elec, year = year,
                                 date = date, prec_round = prec_round,
                                 short_version = short_version,
                                 repo_url = repo_url, file_ext = file_ext,
                                 verbose = FALSE)

    }

    if (is.null(ballots_data)) {

      if (verbose) {

        # Print the file URL for debugging purposes
        message(blue("📦 Import candidacies and ballots data (at poll station level) ..."))
        Sys.sleep(1/20)

        message(magenta("⏳ Please wait, the volume of data downloaded and the internet connection may take a few seconds"))

      }

      ballots_data <-
        import_candidacies_data(type_elec = type_elec, year = year,
                                date = date,
                                repo_url = repo_url, file_ext = file_ext,
                                short_version = short_version,
                                verbose = FALSE)

    }

    # Check if col_id_elec and col_id_elec_level exist within the data
    if (!all(c(col_id_elec, col_id_poll_station) %in% names(election_data)) |
        !all(c(col_id_elec, col_id_poll_station) %in% names(ballots_data))) {

      stop(red("😵 Columns provided in `col_id_elec`and `col_id_poll_station_station` should be available in both tables."))

    }

    # id variables
    group_vars <- c(col_id_elec, col_id_poll_station)

    # join data (without aggregate, just at poll station level)
    join_data <-
      election_data |>
      left_join(ballots_data, by = group_vars,
                suffix = c("", ".rm")) |>
      select(-contains("rm"))

    if (short_version & verbose) {

      message(yellow("⚠️ A short version was asked. If you require all variables, please run with `short_version = FALSE'"))

    }

    # output
    return(join_data)

  }

# agg_nat <- election_data |> aggregate_election_data(level = "all")
# agg_ccaa <- election_data |> aggregate_election_data(level = "ccaa")
# agg_prov <- election_data |> aggregate_election_data(level = "prov")
# agg_mun <- election_data |> aggregate_election_data(level = "mun")
#
# elections_data |>
#   aggregate_election_data() |>
#   select(-pop_res_all, -census_counting_all) |>
#   bind_rows(
#     elections_data |>
#     aggregate_election_data(level = "ccaa") |>
#     summarise(across(blank_ballots:n_poll_stations, sum), .by = id_elec)) |>
#   bind_rows(
#     elections_data |>
#       aggregate_election_data(level = "prov") |>
#       summarise(across(blank_ballots:n_poll_stations, sum), .by = id_elec)) |>
#   bind_rows(
#     elections_data |>
#       aggregate_election_data(level = "mun") |>
#       summarise(across(blank_ballots:n_poll_stations, sum), .by = id_elec)) |>
#   bind_rows(
#     elections_data |>
#       aggregate_election_data(level = "mun_district") |>
#       summarise(across(blank_ballots:n_poll_stations, sum), .by = id_elec)) |>
#   bind_rows(
#     elections_data |>
#       aggregate_election_data(level = "sec") |>
#       summarise(across(blank_ballots:n_poll_stations, sum), .by = id_elec)) |>
#   distinct()

#' @title Aggregate elections data at provided level (ccaa, prov, etc)
#'
#' @description pending...
#'
#' @inheritParams import_poll_station_data
#' @inheritParams import_candidacies_data
#' @inheritParams get_election_data
#' @param level A string providing the level of aggregation at which
#' the data is to be provided. The allowe values are the following:
#' 'all', 'ccaa', 'prov', 'mun', 'mun_district', 'sec' or
#' 'poll_station'. Defaults to \code{"all"}.
#' @param by_parties A flag indicates whether user wants a summary by
#' candidacies/parties or just global results at given \code{level}.
#' Defaults to \code{FALSE}.
#' @param cols_mun_var A vector of variable names that, in their raw
#' version, are only available at the municipal level (or higher).
#' Defaults to \code{c("pop_res_mun", "census_counting_mun")}.
#' @param col_id_candidacies A string indicating the name of the
#' column that uniquely identifies the candidacies. Defaults to
#' \code{"id_candidacies"}.
#' @param cols_names_candidacies A string indicating the name of the
#' column containg the names and/or acronyms of candidacies.
#' Defaults to \code{c("abbrev_candidacies", "name_candidacies")}.
#'
#' @return A tibble with rows corresponding to municipalities for
#' each election, including the following variables:
#' \item{id_elec}{election's id constructed from the election code
#' \code{cod_elec} and date \code{date_elec}.}
#' \item{cod_elec}{code representing the type of election:
#' \code{"01"} (referendum), \code{"02"} (congress),
#' \code{"03"} (senate), \code{"04"} (local elections),
#' \code{"06"} (cabildo - Canarian council - elections), \code{"07"}
#' (European Parliament elections).}
#' \item{type_elec}{type of election.}
#' \item{date_elec}{date of the election.}
#' \item{id_INE_poll_station}{poll station's id constructed from the
#' ccaa-prov-municipality and poll station codes.}
#' \item{id_INE_mun}{municipality ID constructed from the
#' ccaa-prov-mun codes provided by INE.}
#' \item{cod_INE_ccaa, ccaa}{codes and names for regions (ccaa)
#' to which the municipalities belong.}
#' \item{cod_INE_prov, prov}{codes and names for the provinces to
#' which the municipalities belong.}
#' \item{cod_INE_mun, mun}{code, and name for
#' municipalities.}
#' \item{cod_mun_district, cod_sec, cod_poll_station}{codes for the
#' municipal district, census tract and poll station.}
#' \item{ballots_1, turnout_1}{number of total ballots and turnout
#' percentage in the first round.}
#' \item{ballots_2, turnout_2}{number of total ballots and turnout
#' percentage in the second round (if applicable).}
#' \item{blank_ballots, invalid_ballots}{blank and invalid ballots.}
#' \item{party_ballots, valid_ballots, total_ballots}{ballots to
#' candidacies/parties, valid ballots (sum of \code{blank_ballots} and
#' \code{party_ballots}) and total ballots (sum of
#' \code{valid_ballots} and \code{invalid_ballots}).}
#' \item{turnout}{final turnout percentage.}
#' \item{porc_valid, porc_invalid, porc_parties, porc_blank}{perc (%)
#' values of \code{valid_ballots}, \code{invalid_ballots},
#' \code{party_ballots} and \code{blank_ballots}.}
#' \item{n_poll_stations}{number of polling stations.}
#' \item{pop_res_mun}{population census of residents (CER + CERA) at
#' municipality level.}
#' \item{census_counting_mun}{population eligible to vote after
#' claims at municipality level.}
#' \item{id_candidacies}{id for candidacies (at province level).}
#' \item{id_candidacies_ccaa, id_candidacies_nat}{id for
#' candidacies (at region - ccaa - and national level).}
#' \item{abbrev_candidacies, name_candidacies}{acronym and full name
#' of the candidacies.}
#' \item{ballots}{number of ballots obtained for each candidacy at
#' each poll station.}
#'
#' @details This function fetches municipal-level data for the
#' specified elections by downloading the corresponding `.rda` files
#' from GitHub \url{https://github.com/dadosdelaplace/pollspain-data}
#' (directly downloaded from MIR website) and processing them into a
#' tidy format. It automatically handles the download, loading, and
#' merging of data across multiple election periods as specified by
#' the user.
#'
#' @author Javier Álvarez-Liébana and David Pereiro-Pol.
#' @keywords get_elections_data
#' @name aggregate_election_data
#' @import crayon
#' @examples
#'
#' \dontrun{
#' ## Correct examples
#'
#' # Congress elections in April 2019
#' # Fetch elections data for the congress elections in April 2019
#' elections_data1 <- join_election_data("congress", 2019, 4)
#'
#' # Fetch join_election_data("congress", 2019, 4, prec_round = 7)
#' # data for the congress elections in April 2019
#' # with percentages rounded to 7 decimals
#' elections_data2 <-
#'   join_election_data("congress", 2019, 4, prec_round = 7)
#'
#' # Fetch elections data for the congress and senate elections in
#' # April 2019 in a short version
#' elections_data3 <-
#'   join_election_data(c("congress", "senate"), 2019, 4, short_version = TRUE)
#'
#' # Example usage to combine data from different elections into one table
#' # Fetch elections data for congress elections in Nov 2019 and June 2016
#' combined_elections_data <-
#'   join_election_data(c("congress", "congress"),
#'                      c(2019, 2016), c(11, 6))
#'
#' # Example usage providing date instead of month and year
#' date_example <- join_election_data("congress", date = "2016-06-26")
#'
#' # Example usage providing external tables
#' election_data <- import_poll_station_data("congress", 2019, 4)
#' ballots_data <- import_candidacies_data("congress", 2019, 4)
#' join_election_data("congress", 2019, 4,
#'                    election_data = election_data,
#'                    ballots_data = ballots_data)
#'
#' # Example usage providing external tables with different
#' # column names
#' election_data <-
#'   import_poll_station_data("congress", 2019, 4) |>
#'   dplyr::rename(invent_1 = id_elec, invent_2 = id_INE_poll_station)
#' ballots_data <-
#'   import_candidacies_data("congress", 2019, 4) |>
#'   dplyr::rename(invent_1 = id_elec, invent_2 = id_INE_poll_station)
#' join_data <-
#'   join_election_data("congress", 2019, 4,
#'                      election_data = election_data,
#'                      ballots_data = ballots_data,
#'                      col_id_elec = "invent_1",
#'                      col_id_poll = "invent_2")
#'
#' # ----
#' # Incorrect examples
#' # ----
#'
#' # Wrong examples
#'
#' # Invalid election type: "national" is not a valid election type
#' join_election_data("national", 2019, 4)
#'
#' # Invalid election: no congress elections are available in 5-2019.
#' # Please check dataset dates_elections_spain
#' join_election_data("congress", 2019, 5)
#'
#' # Invalid date format: date should be in %Y-%m-%d format
#' join_election_data("congress", date = "26-06-2016")
#'
#' # Invalid short version flag: short_version should be a
#' # logical variable
#' join_election_data("congress", 2019, 4, short_version = "yes")
#'
#' # Invalid include_candidates flag: include_candidates should be a
#' # logical variable
#' join_election_data("congress", 2019, 4, include_candidates = "no")
#'
#' # Invalid key columns: col_id_elec and col_id_poll should be
#' # columns included in datasets election_data and ballots_data
#' election_data <- import_poll_station_data("congress", 2019, 4)
#' ballots_data <- import_candidacies_data("congress", 2019, 4)
#' join_election_data("congress", 2019, 4,
#'                    election_data = election_data,
#'                    ballots_data = ballots_data,
#'                    col_id_elec = "invent_1",
#'                    col_id_poll = "invent_2")
#' }
#'
#' @export
aggregate_election_data <-
  function(election_data, level = "all", by_parties = FALSE,
           col_id_elec = "id_elec",
           col_id_poll_station = "id_INE_poll_station",
           cols_mun_var = c("pop_res_mun", "census_counting_mun"),
           col_id_candidacies = "id_candidacies",
           cols_names_candidacies = c("abbrev_candidacies", "name_candidacies"),
           prec_round = 3,
           verbose = TRUE) {

    # Check if verbose is correct
    if (!is.logical(verbose) | is.na(verbose)) {

      stop(red("😵 `verbose` argument should be a TRUE/FALSE logical flag."))

    }

    if (verbose) {

      message(yellow("🔎 Check if parameters are allowed..."))
      Sys.sleep(1/20)

    }

    # Check if election_data is a data.frame or tibble
    if ((!is.data.frame(election_data) & !is_tibble(election_data)) |
        nrow(election_data) == 0 | ncol(election_data) == 0) {

      stop(red("😵 `election_data` must be a tibble or data.frame whose number of rows and columns should be greater than 0."))

    }

    # Check if level takes allowed values
    if (!(level %in% c("all", "ccaa", "prov", "mun",
                       "mun_district", "sec", "poll_station"))) {

      stop(red("😵 Aggregation level provided by 'level' parameter should be taken from the following values: 'all', 'ccaa', 'prov', 'mun', 'mun_district', 'sec', 'poll_station'"))

    }

    # Check if col_id_elec and col_id_elec_level exist within the data
    if (!all(c(col_id_elec, col_id_poll_station) %in% names(election_data))) {

      stop(red("😵 Columns provided in `col_id_elec` and `col_id_poll_station` should be available in the table."))

    }

    # Check if cols_mun_var exist within the data
    if (!all(cols_mun_var %in% names(election_data))) {

      stop(red("😵 Columns provided in `cols_mun_var` should be available in the table."))

    }


    # Check if prec_round is a positive number
    if (prec_round != as.integer(prec_round) | prec_round < 1) {

      stop(red("😵 Parameter 'prec_round' must be a positive integer greater than 0"))

    }

    # Check: if by_parties is a logical variable
    if (!is.logical(by_parties)) {

      stop(red("😵 Parameter 'by_parties' must be a logical variable, just TRUE/FALSE are avoided"))

    }

    # Check duplicates (without candidacies data)
    election_nodup_data <-
      election_data |>
      distinct(.data[[col_id_elec]], .data[[col_id_poll_station]],
               .data[[col_id_candidacies]], .keep_all = TRUE)

    if (nrow(election_nodup_data) != nrow(election_data)) {

      message(yellow(glue("⚠️ {nrow(election_data) - nrow(election_nodup_data)} duplicates were found and removed.")))

    }
    election_data <- election_nodup_data

    # define factor levels
    hierarchy_levels <-
      factor(c("ccaa", "prov", "mun", "mun_district",
               "sec", "poll_station"),
             levels = c("poll_station", "sec", "mun_district",
                        "mun", "prov", "ccaa"),
             ordered = TRUE)

    # variables to be grouped (for general and mun variables)
    group_var <- group_var_mun <- col_id_elec
    if (level != "all") {

      group_var <-
        c(group_var, glue("cod{if_else(hierarchy_levels[hierarchy_levels >= level] >= 'mun', '_INE', '')}_{hierarchy_levels[hierarchy_levels >= level]}"))
      group_var_mun <-
        c(group_var_mun, glue("cod_INE_{hierarchy_levels[hierarchy_levels >= level & hierarchy_levels >= 'mun']}"))

      for (i in 2:length(group_var)) {
        if (!any(names(election_data) == group_var[i])) {

          election_data <-
            election_data |>
            mutate(!!group_var[i] :=
                     extract_code(.data[[col_id_poll_station]],
                                  level = as.character(hierarchy_levels[i - 1]),
                                  full_cod = FALSE))

        }
      }
    }

    if (any(names(election_data) == "id_INE_mun")) {

      col_id_mun <- "id_INE_mun"

    } else {

      election_data <-
        election_data |>
        mutate("id_INE_mun" =
                 extract_code(.data[[col_id_poll_station]],
                              level = "mun", full_cod = TRUE))
      col_id_mun <- "id_INE_mun"
    }

    if (!by_parties) {

      agg_data <-
        election_data |>
        distinct(.data[[col_id_elec]], .data[[col_id_poll_station]],
                 .keep_all = TRUE) |>
        summarise(across(c(contains("ballots"), -ballots),
                         function(x) { sum(x, na.rm = TRUE) }),
                  n_poll_stations = n_distinct(.data[[col_id_poll_station]]),
                  .by = group_var) |>
        left_join(election_data |>
                    distinct(.data[[col_id_elec]], .data[[col_id_mun]],
                             .keep_all = TRUE) |>
                    summarise(across(cols_mun_var,
                                     function(x) { sum(x, na.rm = TRUE) }),
                              .by = group_var_mun),
                  by = group_var_mun)

    } else {

      poll_data <-
        election_data |>
        distinct(.data[[col_id_elec]], .data[[col_id_poll_station]],
                 .keep_all = TRUE) |>
        summarise(across(c(contains("ballots"), -ballots),
                         function(x) { sum(x, na.rm = TRUE) }),
                  n_poll_stations = n_distinct(.data[[col_id_poll_station]]),
                  .by = group_var)

      agg_data <-
        poll_data |>
        left_join(election_data |>
                    distinct(.data[[col_id_elec]], .data[[col_id_poll_station]],
                             .data[[col_id_candidacies]],
                             .keep_all = TRUE) |>
                    summarise("ballots" = sum(ballots, na.rm = TRUE),
                              .by = c(group_var, col_id_candidacies, cols_names_candidacies)),
                  by = group_var) |>
        left_join(election_data |>
                    distinct(.data[[col_id_elec]], .data[[col_id_mun]],
                             .keep_all = TRUE) |>
                    summarise(across(cols_mun_var,
                                     function(x) { sum(x, na.rm = TRUE) }),
                              .by = group_var_mun),
                  by = group_var_mun)

    }

    # if level is greater than mun, mun_variables have been aggregated
    # at level provided
    if (min(hierarchy_levels[hierarchy_levels >= level]) >= "mun" |
        level == "all") {

      names(agg_data) <-
        str_replace_all(names(agg_data), "_mun", glue("_{level}"))
    }

    return(agg_data)
}

#' @title Summaries of the electoral and candidacies ballots data for
#' a given aggregation level (ccaa, prov, etc)
#'
#' @description pending...
#'
#' @inheritParams import_poll_station_data
#' @inheritParams import_candidacies_data
#' @inheritParams get_election_data
#' @inheritParams aggregate_election_data
#' @param filter_porc_ballots A numerical argument representing the
#' vote percentage threshold (out of 100) that the user wants to use
#' to filter the parties (as long as \code{by_parties = TRUE}).
#' Defaults to \code{NA}.
#'
#' @return A tibble with rows corresponding to ...pending
#'
#' @details This function ... pending
#'
#' @author Javier Álvarez-Liébana and David Pereiro-Pol.
#' @keywords get_elections_data
#' @name summary_election_data
#' @import crayon
#' @examples
#'
#' ## Correct examples
#'
#' # Summary election data at national level (general data
#' # without candidacies ballots)
#' summary_data_all <- summary_election_data("congress", 2019, 4)
#'
#' # Summary election data at national level, aggregating the
#' # candidacies ballots
#' summary_data_all_parties <-
#'   summary_election_data("congress", 2019, 4, by_parties = TRUE)
#'
#' # Summary election data at ccaa level (general data
#' # without candidacies ballots)
#' summary_data_ccaa <-
#'   summary_election_data("congress", 2019, 4, level = "ccaa")
#'
#' # Summary election data at ccaa level, aggregating the
#' # candidacies ballots
#'summary_data_ccaa_parties <-
#'   summary_election_data("congress", 2019, 4, level = "ccaa",
#'                         by_parties = TRUE)
#'
#' \dontrun{
#' # ----
#' # Incorrect examples
#' # ----
#'
#' # Wrong examples
#'
#' }
#'
#' @export
summary_election_data <-
  function(type_elec, year = 2019, date = NULL,
           election_data = NULL, ballots_data = NULL,
           col_id_elec = "id_elec", col_id_poll_station = "id_INE_poll_station",
           prec_round = 3, short_version = TRUE,
           repo_url = "https://github.com/dadosdelaplace/pollspain-data/blob/main",
           file_ext = ".rda", level = "all", by_parties = FALSE,
           filter_porc_ballots = NA,
           cols_mun_var = c("pop_res_mun", "census_counting_mun"),
           col_id_candidacies = "id_candidacies",
           cols_names_candidacies = c("abbrev_candidacies", "name_candidacies"),
           verbose = TRUE) {

    # Check if verbose is correct
    if (!is.logical(verbose) | is.na(verbose)) {

      stop(red("😵 `verbose` argument should be a TRUE/FALSE logical flag."))

    }

    # check filter_porc_ballots
    if (!(is.na(filter_porc_ballots) |
          (is.numeric(filter_porc_ballots) & filter_porc_ballots > 0 & filter_porc_ballots < 100))) {

      stop(red("😵 `filter_porc_ballots` argument should be NA or a numeric value between 0 and 100."))

    }

    if (verbose) {

      message(yellow("🔎 Check if parameters are allowed..."))
      Sys.sleep(1/20)

    }

    if (verbose) {

      message(blue("📦 Import data at poll station level ..."))
      Sys.sleep(1/20)

      message(magenta("⏳ Please wait, the volume of data downloaded and the internet connection may take a few seconds"))


    }

    election_data <-
      get_election_data(type_elec = type_elec, year = year,
                        date = date,
                        election_data = NULL, ballots_data = NULL,
                        col_id_elec = col_id_elec, col_id_poll_station = col_id_poll_station,
                        prec_round = prec_round, short_version = short_version,
                        repo_url = repo_url, file_ext = file_ext, verbose = FALSE)

    if (verbose) {

      message(blue(glue("   🗺 Aggregate data at {ifelse(level == 'all', 'national', level)} level ...")))
      Sys.sleep(1/20)

    }

    summary_data <-
      election_data |>
      aggregate_election_data(level = level, by_parties = by_parties,
                              col_id_elec = col_id_elec,
                              col_id_poll_station = col_id_poll_station,
                              cols_mun_var = cols_mun_var,
                              col_id_candidacies = col_id_candidacies,
                              cols_names_candidacies = cols_names_candidacies,
                              prec_round = prec_round,
                              verbose = FALSE)

    if (verbose) {

      message(bgYellow(black("✅🖇 Join information sources and last summaries ...\n")))
      Sys.sleep(1/20)

    }

    if (by_parties) {

      # Including some summaries
      census_var <-
        names(summary_data)[str_detect(names(summary_data), "census_counting")]
      summary_data  <-
        summary_data  |>
        mutate("porc_candidacies_parties" =
                 round(100*ballots/party_ballots, prec_round),
               "porc_candidacies_valid" =
                 round(100*ballots/valid_ballots, prec_round),
               "porc_candidacies_census" =
                 round(100*ballots/.data[[census_var]], prec_round))

    }

    if (!is.na(filter_porc_ballots)) {

      if (!by_parties) {

        message(yellow("⚠️ If `by_parties` is FALSE, a filtering by percentage of party ballots cannot be achieved"))

      } else {

        agg_data <-
          agg_data |>
          filter(porc_candidacies_valid >= filter_porc_ballots)

      }
    }

    return(summary_data)
}



# ¿arreglar CERA?


