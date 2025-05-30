
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
#' @param col_id_mun Column name in \code{election_data} to
#' uniquely identify municipalities. Defaults to
#' \code{"id_INE_mun"}.
#' @param col_id_candidacies A named vector (with names "id_prov" and
#' "id_nat") in which user provides the column names for the
#' candidacies id at province level and national level. Defaults to
#' c("id_prov" = "id_candidacies", "id_nat" = "id_candidacies_nat").
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
#' \item{date_elec}{date of the election. Variable available only for
#' long version.}
#' \item{id_INE_poll_station}{poll station's id constructed from the
#' ccaa-prov-municipality and poll station codes.}
#' \item{id_INE_mun}{municipality ID constructed from the
#' ccaa-prov-mun codes provided by INE.}
#' \item{cod_INE_ccaa, ccaa}{codes and names for regions (ccaa)
#' to which the municipalities belong. Codes available only for
#' long version.}
#' \item{cod_INE_prov, prov}{codes and names for the provinces to which
#' the municipalities belong. Codes available only for long version.}
#' \item{cod_INE_mun, mun}{code, and name for
#' municipalities. Codes available only for long version.}
#' \item{cod_mun_district, cod_sec, cod_poll_station}{codes for the
#' municipal district, census tract and poll station. They are
#' only available for long version.}
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
#' \item{turnout}{final turnout percentage. It is only
#' available for long version.}
#' \item{porc_valid, porc_invalid, porc_parties, porc_blank}{perc (%)
#' values of \code{valid_ballots}, \code{invalid_ballots},
#' \code{party_ballots} and \code{blank_ballots}.}
#' \item{n_poll_stations}{number of polling stations. It is only
#' available for long version.}
#' \item{census_counting_mun}{population eligible to vote after claims
#' at municipality level.}
#' \item{pop_res_mun}{population census of residents (CER + CERA) at
#' municipality level.}
#' \item{id_candidacies}{id for candidacies at province level.}
#' \item{id_candidacies_ccaa, id_candidacies_nat}{id for candidacies
#' at ccaa level (only provided for long version) and at national
#' level.}
#' \item{abbrev_candidacies, name_candidacies}{acronym and full name
#' of the candidacies. They are only available for long version.}
#' \item{ballots}{number of ballots obtained for each candidacy at
#' each poll station.}
#'
#' @details The purpose of this function is to easily combine the
#' outputs of \code{import_poll_station_data()},
#' \code{import_mun_census_data()}, and
#' \code{import_candidacies_data()}, providing a tibble at the
#' polling station level with both general voting data (blank votes,
#' null votes, etc.) and votes for each candidacy (identified by
#' their corresponding ids). This function does not perform
#' aggregations and is not intended as a final-use tool for basic
#' users, but rather as an intermediate step for the
#' \code{summary_election_data()} function.
#'
#' @author Javier Alvarez-Liebana and David Pereiro-Pol.
#' @keywords get_elections_data
#' @name get_election_data
#' @import crayon
#' @examples
#'
#' ## Correct examples
#'
#' # Congress elections in year 2008, 2016 and "2023-07-24"
#' # in a short version
#' elections_data <-
#'   get_election_data(type_elec = "congress", year = c(2008, 2016),
#'                     date = "2023-07-24")
#'
#' # Congress elections in 2008 and "1989-10-29"
#' # in a long version
#' elections_data <-
#'   get_election_data(type_elec = "congress", year = 2008,
#'                     date = "1989-10-29", short_version = FALSE)
#'
#' # Example usage providing external tables
#' election_data <-
#'   import_poll_station_data(type_elec = "congress", year = 2016) |>
#'   dplyr::rename(invent_1 = id_elec, invent_2 = id_INE_poll_station)
#' ballots_data <-
#'   import_candidacies_data(type_elec = "congress", year = 2016) |>
#'   dplyr::rename(invent_1 = id_elec, invent_2 = id_INE_poll_station)
#' join_data <-
#'   get_election_data(type_elec = "congress",
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
#' get_election_data(type_elec = "national", year = 2019)
#'
#' # Invalid date format: date should be in %Y-%m-%d format
#' get_election_data(type_elec = "congress", date = "26-06-2016")
#'
#' # Invalid short version flag: short_version should be a
#' # logical variable
#' get_election_data(type_elec = "congress", year = 2019,
#'                   short_version = "yes")
#'
#' # Invalid key columns: col_id_elec and col_id_poll should be
#' # columns included in datasets election_data and ballots_data
#' election_data <-
#'   import_poll_station_data(type_elec = "congress", year = 2019)
#' ballots_data <-
#'   import_candidacies_data(type_elec = "congress", year = 2019)
#' get_election_data(type_elec = "congress", year = 2019,
#'                    election_data = election_data,
#'                    ballots_data = ballots_data,
#'                    col_id_elec = "invent_1",
#'                    col_id_poll_station = "invent_2")
#' }
#'
#' @export
get_election_data <-
  function(type_elec, year = NULL, date = NULL,
           election_data = NULL, ballots_data = NULL,
           col_id_elec = "id_elec",
           col_id_poll_station = "id_INE_poll_station",
           col_id_mun = "id_INE_mun",
           col_id_candidacies = c("id_prov" = "id_candidacies",
                                  "id_nat" = "id_candidacies_nat"),
           prec_round = 3, short_version = TRUE, verbose = TRUE,
           lazy_duckdb = FALSE, con_duckdb = NULL) {

    # Check if verbose is correct
    if (!is.logical(verbose) | is.na(verbose)) {

      stop(red("Ups! `verbose` argument should be a TRUE/FALSE logical flag."))

    }

    if (verbose) {

      message(yellow("... Check if parameters are allowed..."))
      Sys.sleep(1/20)

    }

    # for the moment, just congress election
    if (!all(type_elec %in% c("congress", "senate"))) {

      stop(red("Ups! The package is currently under development so, for the moment, it only allows access to congress and senate data."))

    }

    # check short_version
    if (!is.logical(short_version) | is.na(short_version)) {

      stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

    }

    if (!is.null(election_data)) {
      if (!all(col_id_candidacies %in% names(ballots_data))) {

        stop(red("Ups! Variables names in `col_id_candidacies` should be matched with column names in ballots_data"))

      }
    }
    # Check date
    if (is.null(election_data)) {
      if (!is.null(date)) {

        if (!all(str_detect(date, "^\\d{4}-\\d{2}-\\d{2}$")) | any(is.na(date))) {

          stop(red("Ups! If date was provided, `date` should be in format '2000-01-01' (%Y-%m-%d)"))

        } else {
          date <- as_date(date)
        }
      } else {
        if (!is.numeric(year)) {
          stop(red("Ups! If no date was provided, `year` should be a numerical variable."))
        }
      }
    }

    # Design a tibble with all elections asked by user
    # Ensure input parameters are vectors
    if (is.null(election_data)) {
      if (!is.null(year)) {

        asked_elections <-
          expand_grid(as.vector(type_elec), as.vector(year))
        names(asked_elections) <- c("type_elec", "year")
        asked_elections <-
          asked_elections |>
          distinct(type_elec, year) |>
          mutate("cod_elec" = type_to_code_election(as.vector(type_elec)),
                 .before = everything())

        allowed_elections <-
          dates_elections_spain |>
          inner_join(asked_elections,
                     by = c("cod_elec", "type_elec", "year"),
                     suffix = c("", ".rm")) |>
          select(-contains("rm")) |>
          distinct(cod_elec, type_elec, date, .keep_all = TRUE)

      }

      if (!is.null(date)) {

        asked_elections_date <-
          expand_grid(as.vector(type_elec), as_date(date)) |>
          mutate("year" = year(date))
        names(asked_elections_date) <- c("type_elec", "date", "year")
        asked_elections_date <-
          asked_elections_date |>
          distinct(type_elec, date, year) |>
          mutate("cod_elec" = type_to_code_election(as.vector(type_elec)),
                 .before = everything())

        allowed_elections_date <-
          dates_elections_spain |>
          inner_join(asked_elections_date,
                     by = c("cod_elec", "type_elec", "date"),
                     suffix = c("", ".rm")) |>
          select(-contains("rm")) |>
          distinct(cod_elec, type_elec, date, .keep_all = TRUE)
      }

      if (!is.null(year)) {
        if (!is.null(date)) {

          allowed_elections <-
            allowed_elections |>
            bind_rows(allowed_elections_date) |>
            distinct(cod_elec, type_elec, date, .keep_all = TRUE)
        }
      } else {

        allowed_elections <- allowed_elections_date

        if (!is.null(date)) {

          allowed_elections <-
            allowed_elections_date |>
            bind_rows(allowed_elections) |>
            distinct(cod_elec, type_elec, date, .keep_all = TRUE)
        }
      }


      ambiguous_years <- intersect(allowed_elections$year, 2019)
      chosen_dates <- NULL

      if (length(ambiguous_years) > 0 & is.null(date)) {

        normal_years <- setdiff(year, 2019)
        normal_dates <- dates_elections_spain |>
          filter(type_elec %in% !!type_elec & year %in% normal_years) |>
          pull(date)

        normal_dates <- as.character(normal_dates)

        if (interactive()) {

          sel <- menu(c("April (2019-04-28)",
                        "November (2019-11-10)",
                        "Both dates"),
                      title = paste0("What 2019 election do you want?"))

          chosen_dates <- c(
            chosen_dates,
            switch(sel,
                   "2019-04-28",
                   "2019-11-10",
                   c("2019-04-28", "2019-11-10"))
          )
        } else {

          chosen_dates <- c(chosen_dates, c("2019-04-28", "2019-11-10"))
        }

        dates_ok <- as_date(unique(c(date, normal_dates, chosen_dates)))

      } else {

        normal_dates <- dates_elections_spain |>
          filter(type_elec %in% !!type_elec &
                   year %in% allowed_elections$year) |>
          pull(date)

        normal_dates <- as.character(normal_dates)
        dates_ok <- as_date(unique(c(date, normal_dates)))

      }
      allowed_elections <- allowed_elections |> filter(date %in% dates_ok)

      if (verbose) {

        # Print the file URL for debugging purposes
        message(blue("[x] Import poll station data ..."))
        Sys.sleep(1/20)
        message(green(paste0("   ... ", allowed_elections$type_elec, " elections on ", allowed_elections$date, "\n")))

      }
    }

   # Import the data
    if (is.null(con_duckdb)) {

      con <- .get_duckdb_con()

    } else {

      con <- con_duckdb

    }

    if (is.null(election_data)) {

      election_data <-
        import_poll_station_data(type_elec = type_elec, year = NULL,
                                 date = allowed_elections$date, prec_round = prec_round,
                                 short_version = FALSE,
                                 verbose = FALSE, lazy_duckdb = TRUE,
                                 con_duckdb = con)

    }

    if (verbose) {

      # Print the file URL for debugging purposes
      message(blue("[x] Import candidacies and ballots data (at poll station level) ..."))
      Sys.sleep(1/20)
      message(magenta("   ... please wait, volume of data downloaded and internet connection may take a few seconds"))

    }

    if (is.null(ballots_data)) {

      ballots_data <-
        import_candidacies_data(type_elec = type_elec, year = NULL,
                                date = allowed_elections$date,
                                short_version = FALSE,
                                verbose = FALSE, lazy_duckdb = TRUE,
                                con_duckdb = con)

    }

    if (!any(dbListTables(con) == "election_data")) {

      copy_to(con, election_data, name = "election_data", temporary = FALSE)

    }

    if (!any(dbListTables(con) == "ballots_data")) {

      copy_to(con, ballots_data, name = "ballots_data", temporary = FALSE)

    }

    # Check if col_id_elec and col_id_elec_level exist within the data
    if (!all(c(col_id_elec, col_id_poll_station) %in% colnames(election_data)) |
        !all(c(col_id_elec, col_id_poll_station) %in% colnames(ballots_data))) {

      stop(red("Ups! Columns provided in `col_id_elec`and `col_id_poll_station_station` should be available in both tables."))

    }

    # id variables
    group_vars <- c(col_id_elec, col_id_poll_station)

    # join data (without aggregate, just at poll station level)
    join_data <-
      tbl(con, "election_data") |>
      left_join(tbl(con, "ballots_data"), by = group_vars,
                suffix = c("", ".rm")) |>
      select(-contains("rm"))

    # remove memory
    rm(list = c("election_data", "ballots_data"))
    gc()

    # Check summaries
    check_totals <-
      join_data |>
      summarise("sum_party_ballots" = sum(ballots),
                # unique not available in duckdb
                "party_ballots" = min(party_ballots),
                .by = all_of(group_vars)) |>
      filter(sum_party_ballots != party_ballots)

    if (nrow(check_totals |> collect()) > 0) {
      if (verbose) {

        message(yellow(glue("Be careful! Some poll stations does not match individual ballots with summaries provided by MIR. The discrepancies were resolved by using votes by candidacies.")))

      }

      join_data <-
        join_data |>
        mutate("party_ballots" = sum(ballots), .by = all_of(group_vars)) |>
        mutate("valid_ballots" = party_ballots + blank_ballots,
               "total_ballots" = valid_ballots + invalid_ballots,
               "turnout" = round(100 * total_ballots / census_counting_mun, prec_round),
               "porc_valid" =
                 round(100 * valid_ballots / total_ballots, prec_round),
               "porc_invalid" =
                 round(100 * invalid_ballots / total_ballots, prec_round),
               "porc_parties" = round(100 * party_ballots / valid_ballots, prec_round),
               "porc_blank" = round(100 * blank_ballots / valid_ballots, prec_round))
    }

    if (short_version) {

      if (verbose) {
        message(yellow("\nA short version was asked (if you want all variables, run with `short_version = FALSE`)"))
      }

      join_data <-
        join_data |>
        select(all_of(c(col_id_elec, "type_elec",
                        col_id_poll_station,
                        col_id_mun)),
               ccaa, prov, mun, blank_ballots,
               invalid_ballots, party_ballots, valid_ballots,
               total_ballots, turnout, porc_valid, porc_invalid,
               porc_parties, porc_blank, pop_res_mun, census_counting_mun,
               ballots,
               all_of(c(col_id_candidacies[["id_prov"]],
                        col_id_candidacies[["id_nat"]])))
    }

    if (!lazy_duckdb) {

      join_data <- join_data |> collect()
      DBI::dbDisconnect(con, shutdown = TRUE)
    }

    # output
    return(join_data)

 }

#' @title Aggregate elections data at provided level (ccaa, prov, etc)
#'
#' @description Aggregate polling station election results
#' to any chosen territorial level, providing party level ballots,
#' total ballots, number of polling stations and contextual sums.
#'
#' @inheritParams import_poll_station_data
#' @inheritParams import_candidacies_data
#' @inheritParams get_election_data
#' @param election_data A database containing general election data
#' already provided (by other functions or by the user). Database
#' should contain \code{col_id_elec}, \code{col_id_poll_station},
#' \code{cols_mun_var} and \code{col_id_candidacies} columns.
#' Defaults to \code{NULL}.
#' @param level A string providing the level of aggregation at which
#' the data is to be provided. The allowed values are the following:
#' 'all', 'ccaa', 'prov', 'mun', 'mun_district', 'sec' or
#' 'poll_station'. Defaults to \code{"all"}.
#' @param by_parties A flag indicates whether user wants a summary by
#' candidacies/parties or just global results at given \code{level}.
#' Defaults to \code{TRUE}.
#' @param cols_mun_var A vector of variable names that, in their raw
#' version, are only available at the municipal level (or higher).
#' Defaults to \code{c("pop_res_mun", "census_counting_mun")}.
#' @param col_id_candidacies A named vector (with names "id_prov" and
#' "id_nat") in which user provides the column names for the
#' candidacies id at province level and national level. Defaults to
#' c("id_prov" = "id_candidacies", "id_nat" = "id_candidacies_nat").
#'
#' @return A tibble with rows corresponding to the level of aggregation for
#' each election, including the following variables:
#' \item{id_elec}{election's id constructed from the election code
#' \code{cod_elec} and date \code{date_elec}.}
#' \item{cod_elec}{code representing the type of election:
#' \code{"01"} (referendum), \code{"02"} (congress),
#' \code{"03"} (senate), \code{"04"} (local elections),
#' \code{"06"} (cabildo - Canarian council - elections), \code{"07"}
#' (European Parliament elections).}
#' \item{id_INE_xxx}{id for the xxx constituency provided in
#' \code{level}: id_INE_ccaa, id_INE_prov, etc.}
#' \item{xxx}{names for the xxx constituency provided in
#' \code{level}: ccaa, prov, etc.}
#' \item{blank_ballots, invalid_ballots}{blank and invalid ballots.}
#' \item{party_ballots, valid_ballots, total_ballots}{ballots to
#' candidacies/parties, valid ballots (sum of \code{blank_ballots} and
#' \code{party_ballots}) and total ballots (sum of
#' \code{valid_ballots} and \code{invalid_ballots}).}
#' \item{n_poll_stations}{number of polling stations.}
#' \item{pop_res_xxx}{population census of residents (CER + CERA) at
#' xxx level (if level is below municipality level, it is provided at
#' municipality level). It is only available for long version.}
#' \item{census_counting_mun}{population eligible to vote after
#' claims at xxx level (if level is below municipality level, it is
#' provided at municipality level). It is only available for
#' long version.}
#' \item{id_candidacies}{id for candidacies (at province level).}
#' \item{id_candidacies_nat}{id for candidacies at region national
#' level.}
#' \item{ballots}{number of ballots obtained for each candidacy at
#' each level section.}
#'
#' @details This function is actually a helper function that, given
#' an electoral data file with a specific structure, aggregates the
#' information to the level specified in \code{level}. Data that is
#' only available at the provincial or municipal level is handled
#' differently when the aggregation level is below those levels
#' (for example, CERA data cannot be aggregated below the province,
#' in which case 52 special constituencies are added). This function
#' is not intended as a final-use tool for basic users, but rather as
#' an intermediate step for the \code{summary_election_data()}
#' function.
#'
#' @author Javier Alvarez-Liebana and David Pereiro-Pol.
#' @keywords get_elections_data
#' @name aggregate_election_data
#' @import crayon
#' @examples
#' ## Correct examples
#'
#' # Election date from 2023 and 1989 dates
#' election_data <-
#'    get_election_data(type_elec = "congress", year = 2023,
#'                      date = "1989-10-29")
#'
#' # National level results (without parties)
#' nat_agg <- aggregate_election_data(election_data, level = "all",
#'                                    by_parties = FALSE)
#'
#' # Province level results (with parties)
#' prov_agg <- aggregate_election_data(election_data, level = "prov")
#'
#' \dontrun{
#'
#' # ----
#' # Incorrect examples
#' # ----
#'
#' # Wrong examples
#'
#' # Invalid 'level' argument,"district" is not allowed
#' aggregate_election_data(election_data, level = "district")
#'
#' # Invalid 'by_parties' flag: it must be logical, not character
#' aggregate_election_data(election_data, level = "prov",
#'                         by_parties = "yes")
#'
#' # Invalid parameters: col_id_candidacies should be matched with
#' # the variable names
#' aggregate_election_data(election_data, level = "ccaa",
#'                         col_id_candidacies = "wrong_id")
#'
#' }
#'
#' @export
aggregate_election_data <-
  function(election_data, level = "all", by_parties = TRUE,
           col_id_elec = "id_elec",
           col_id_poll_station = "id_INE_poll_station",
           col_id_mun = "id_INE_mun",
           cols_mun_var = c("pop_res_mun", "census_counting_mun"),
           col_id_candidacies = c("id_prov" = "id_candidacies",
                                  "id_nat" = "id_candidacies_nat"),
           prec_round = 3, verbose = TRUE, short_version = TRUE,
           lazy_duckdb = FALSE, con_duckdb = NULL) {

    # Check if verbose is correct
    if (!is.logical(verbose) | is.na(verbose)) {

      stop(red("Ups! `verbose` argument should be a TRUE/FALSE logical flag."))

    }

    if (verbose) {

      message(yellow("   ... check if aggregation parameters are allowed..."))
      Sys.sleep(1/20)

    }


    # check short_version
    if (!is.logical(short_version) | is.na(short_version)) {

      stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

    }

    # Check if level takes allowed values
    if (!(level %in% c("all", "ccaa", "prov", "mun",
                       "mun_district", "sec", "poll_station"))) {

      stop(red("Ups! Aggregation level provided by 'level' parameter should be taken from the following values: 'all', 'ccaa', 'prov', 'mun', 'mun_district', 'sec', 'poll_station'"))

    }

    # Check if col_id_elec and col_id_elec_level exist within the data
    if (!all(c(col_id_elec, col_id_poll_station) %in% colnames(election_data))) {

      stop(red("Ups! Columns provided in `col_id_elec` and `col_id_poll_station` should be available in the table."))

    }

    # Check if cols_mun_var exist within the data
    if (!all(cols_mun_var %in% colnames(election_data))) {

      stop(red("Ups! Columns provided in `cols_mun_var` should be available in the table."))

    }


    # Check if prec_round is a positive number
    if (prec_round != as.integer(prec_round) | prec_round < 1) {

      stop(red("Ups! Parameter 'prec_round' must be a positive integer greater than 0"))

    }

    # Check: if by_parties is a logical variable
    if (!is.logical(by_parties)) {

      stop(red("Ups! Parameter 'by_parties' must be a logical variable, just TRUE/FALSE are avoided"))

    }

    # Check id_candidacies
    if (!all(col_id_candidacies %in% colnames(election_data))) {

      stop(red("Ups! Variables names in `col_id_candidacies` should be matched with column names in electoral_data"))
    }

    # Check duplicates (without candidacies data)
    election_nodup_data <-
      election_data |>
      distinct(.data[[col_id_elec]], .data[[col_id_poll_station]],
               .data[[col_id_candidacies[["id_prov"]]]],
               .data[[col_id_candidacies[["id_nat"]]]], .keep_all = TRUE)
    election_data <- election_nodup_data

    # remove memory
    rm(list = c("election_nodup_data"))
    gc()

    # define factor levels
    hierarchy_levels <-
      factor(c("ccaa", "prov", "mun", "mun_district",
               "sec", "poll_station"),
             levels = c("poll_station", "sec", "mun_district",
                        "mun", "prov", "ccaa"),
             ordered = TRUE)

    if (!(col_id_mun %in% colnames(election_data))) {

      election_data <-
        election_data |>
        mutate(!!col_id_mun :=
                 extract_code(.data[[col_id_poll_station]],
                              level = "mun",
                              full_cod = TRUE))
    }
    # variables to be grouped (for general and mun variables)
    group_var <- group_var_mun <- col_id_elec
    if (level != "all") {

      group_var <-
        c(group_var, paste0("cod", if_else(hierarchy_levels[hierarchy_levels >= level] >= 'mun', '_INE', ''),
                            "_", hierarchy_levels[hierarchy_levels >= level]))
      group_var_mun <-
        c(group_var_mun,
          paste0("cod_INE_",
                 hierarchy_levels[hierarchy_levels >= level & hierarchy_levels >= 'mun']))

      for (i in 2:length(group_var)) {
        if (!any(colnames(election_data) == group_var[i])) {

          election_data <-
            election_data |>
            collect() |>
            mutate(!!group_var[i] :=
                     extract_code(.data[[col_id_poll_station]],
                                  level = as.character(hierarchy_levels[i - 1]),
                                  full_cod = FALSE))

        }
      }
    }

    if (is.null(con_duckdb)) {

      con <- .get_duckdb_con()

    } else {

      con <- con_duckdb

    }
    copy_to(con, election_data, name = "election_data", overwrite = TRUE)
    rm(election_data)
    election_data <- tbl(con, "election_data")

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

      group_candidacies <-
        if_else(level == "all", col_id_candidacies["id_nat"],
                col_id_candidacies["id_prov"])

      aux <-
        election_data |>
        distinct(.data[[col_id_elec]], .data[[col_id_poll_station]],
                 .data[[col_id_candidacies[["id_prov"]]]],
                 .data[[col_id_candidacies[["id_nat"]]]],
                 .keep_all = TRUE) |>
        collect() |>
        summarise("ballots" = sum(ballots, na.rm = TRUE),
                  "id_candidacies" = list(sort(unique(.data[[col_id_candidacies["id_prov"]]]))),
                  "id_candidacies_nat" = list(sort(unique(.data[[col_id_candidacies["id_nat"]]]))),
                  .by = c(group_var, as.character(group_candidacies)))

      copy_to(con, aux, name = "aux", overwrite = TRUE)
      rm(aux)
      agg_data <-
        poll_data |>
        left_join(tbl(con, "aux"),
                  by = group_var)

      if (!short_version) {

        agg_data <-
          agg_data |>
          left_join(election_data |>
                      distinct(.data[[col_id_elec]], .data[[col_id_mun]],
                               .keep_all = TRUE) |>
                      summarise(across(cols_mun_var,
                                       function(x) { sum(x, na.rm = TRUE) }),
                                .by = group_var_mun),
                    by = group_var_mun)
      }
    }


    # join info ccaa-prov-mun from INE
    if (level != "all") {

      agg_data <-
        agg_data |>
        left_join(election_data |>
                    select(-matches("id|cd_INE|MIR")) |>
                    select(matches(str_flatten(as.character(hierarchy_levels[hierarchy_levels >= level]), collapse = "|"))) |>
                    select(-any_of(c("cod_mun_district", "cod_sec", "cod_poll_station",
                                     cols_mun_var))) |>
                    distinct(.keep_all = TRUE),
                  by = c(group_var[group_var %in%
                                     c("cod_INE_ccaa", "cod_INE_prov", "cod_INE_mun")])) |>
        collect() |>
        unite(!!paste0("id_INE_", level), group_var[group_var != col_id_elec],
              sep = "-") |>
        select(col_id_elec, paste0("id_INE_", level),
               any_of(c("ccaa", "prov", "mun")),
               everything())

      copy_to(con, agg_data, name = "agg_data", overwrite = TRUE)
      rm(agg_data)
      agg_data <- tbl(con, "agg_data")

    }

    # remove memory
    rm(list = c("election_data"))
    gc()

    if (by_parties) {
      if (all(lengths(agg_data |> pull(id_candidacies)) == 1)) {

        agg_data <-
          agg_data |>
          mutate("id_candidacies" = unlist(id_candidacies))
      }
    }

    if (by_parties) {
      if (all(lengths(agg_data |> pull(id_candidacies_nat)) == 1)) {

        agg_data <-
          agg_data |>
          mutate("id_candidacies_nat" = unlist(id_candidacies_nat))
      }
    }

    # if level is greater than mun, mun_variables have been aggregated
    # at level provided
    if (min(hierarchy_levels[hierarchy_levels >= level]) >= "mun" |
        level == "all") {

      agg_data <-
        agg_data |>
        rename_with(~ str_replace_all(.x, "_mun", paste0("_", level)))

    }

    if (!lazy_duckdb) {

      agg_data <- agg_data |> collect()
      DBI::dbDisconnect(con, shutdown = TRUE)
    }

    # output
    return(agg_data)
}

#' @title Summaries of the electoral and candidacies ballots data for
#' a given aggregation level (ccaa, prov, etc)
#'
#' @description pending Import, preprocess and aggregate election data at the same time for
#' a given election and aggregation level. This function also lets remove parties below a given
#' vote share threshold.
#'
#' @inheritParams import_poll_station_data
#' @inheritParams import_candidacies_data
#' @inheritParams get_election_data
#' @inheritParams aggregate_election_data
#' @param CERA_remove Flag to indicate whether it should be removed
#' the ballots related to CERA constituencies. Defaults to
#' \code{FALSE}.
#' @param filter_candidacies A string of characters containing
#' party abbreviations which ballots will be filtered (as long as
#' \code{by_parties = TRUE}). Defaults to \code{NA}.
#' @param candidacies_data A database containing the information of
#' candidacies. Database should contain \code{col_abbrev_candidacies}
#' and \code{col_id_candidacies} columns. Defaults to \code{NULL}.
#' @param col_abbrev_candidacies Column name to uniquely identify the
#' party abbreviations. Defaults to \code{"abbrev_candidacies"}.
#' @param filter_porc_ballots A numerical argument representing the
#' vote percentage threshold (out of 100) that the user wants to use
#' to filter the parties (as long as \code{by_parties = TRUE}).
#' Defaults to \code{NA}.
#'
#' @return A tibble with rows corresponding to the level of aggregation for
#' each election, including the following variables:
#' \item{id_elec}{election's id constructed from the election code
#' \code{cod_elec} and date \code{date_elec}.}
#' \item{id_INE_xxx}{id for the xxx constituency provided in
#' \code{level}: id_INE_ccaa, id_INE_prov, etc. It is only provided
#' for long version.}
#' \item{xxx}{names for the xxx constituency provided in
#' \code{level}: ccaa, prov, etc.}
#' \item{ballots_1, ballots_2}{number of total ballots and turnout
#' percentage in the first and second round (if applicable). It is
#' only provided for long version.}
#' \item{blank_ballots, invalid_ballots}{blank and invalid ballots.}
#' \item{party_ballots, valid_ballots, total_ballots}{ballots to
#' candidacies/parties, valid ballots (sum of \code{blank_ballots} and
#' \code{party_ballots}) and total ballots (sum of
#' \code{valid_ballots} and \code{invalid_ballots}).}
#' \item{porc_candidacies_parties, porc_candidacies_valid,
#' porc_candidacies_census}{perc (%) values of \code{ballots} for
#' each candidacy related to \code{party_ballots},
#' \code{valid_ballots} and \code{census_counting_xxx}, respectively.}
#' \item{n_poll_stations}{number of polling stations. It is only
#' provided for long version.}
#' \item{pop_res_xxx}{population census of residents (CER + CERA) at
#' xxx level. It is only provided for long version.}
#' \item{census_counting_xxx}{population eligible to vote after
#' claims at xxx level. It is only provided for long version.}
#' \item{id_candidacies}{id for candidacies: national ids when
#' \code{level = "all"} and province ids otherwise.}
#' \item{abbrev_candidacies, name_candidacies}{acronym and full name
#' of the candidacies.}
#' \item{ballots}{number of ballots obtained for each candidacy at
#' each level section.}
#'
#' @details pending...
#'
#' @author Javier Alvarez-Liebana and David Pereiro-Pol.
#' @keywords get_elections_data
#' @name summary_election_data
#' @import crayon
#' @examples
#'
#' ## Correct examples
#'
#' # Summary 2023 and 2016 election data at prov level,
#' # aggregating the candidacies ballots, in a short version
#' summary_prov <-
#'   summary_election_data(type_elec = "congress", year = 2023,
#'                         level = "prov", date = "2016-06-26")
#'
#' # Summary 2023 election data at mun level, aggregating the
#' # candidacies ballots, in a long version, and filtering ballots
#' # above 45% (percentage between 0 and 100) and just PP and PSOE
#' # parties
#' summary_mun <-
#'   summary_election_data(type_elec = "congress", year = 2023,
#'                         level = "mun", short_version = FALSE,
#'                         filter_candidacies = c("PSOE", "PP"),
#'                         filter_porc_ballots = 45)
#'
#' \dontrun{
#' # ----
#' # Incorrect examples
#' # ----
#'
#' # Wrong examples
#'
#' # Invalid aggregation level
#' summary_election_data("congress", 2019, level = "district")
#'
#' # filter_porc_ballots outside range 0 from 100
#' summary_election_data("congress", 2019,
#'                       filter_porc_ballots = 150)
#'
#' # filter_porc_ballots supplied while by_parties = FALSE
#' summary_election_data("congress", 2019,
#'                       by_parties = FALSE,
#'                       filter_porc_ballots = 5)
#'
#' # Wrong election type
#' summary_election_data("national", 2019)
#' }
#'
#' @export
summary_election_data <-
  function(type_elec, year = NULL, date = NULL,
           election_data = NULL, ballots_data = NULL,
           candidacies_data = NULL,
           col_id_elec = "id_elec",
           col_id_poll_station = "id_INE_poll_station",
           col_id_mun = "id_INE_mun",
           prec_round = 3, short_version = TRUE, CERA_remove = FALSE,
           level = "all", by_parties = TRUE, filter_porc_ballots = NA,
           filter_candidacies = NA,
           cols_mun_var = c("pop_res_mun", "census_counting_mun"),
           col_id_candidacies = c("id_prov" = "id_candidacies",
                                  "id_nat" = "id_candidacies_nat"),
           col_abbrev_candidacies = "abbrev_candidacies",
           verbose = TRUE, lazy_duckdb = FALSE, con_duckdb = NULL) {

    # Check if verbose is correct
    if (!is.logical(verbose) | is.na(verbose)) {

      stop(red("Ups! `verbose` argument should be a TRUE/FALSE logical flag."))

    }

    # check filter_candidacies
    if (!(is.na(filter_porc_ballots) |
          (is.numeric(filter_porc_ballots) & filter_porc_ballots > 0 & filter_porc_ballots < 100))) {

      stop(red("Ups! `filter_porc_ballots` argument should be NA or a numeric value between 0 and 100."))

    }

    # for the moment, just congress election
    if (!all(type_elec %in% c("congress", "senate"))) {

      stop(red("Ups! The package is currently under development so, for the moment, it only allows access to congress and senate data."))

    }

    # check filter_candidacies
    if (!all(is.na(filter_candidacies)) & !all(is.character(filter_candidacies))) {

      stop(red("Ups! `filter_candidacies` argument should be NA or a string of characters."))

    }

    # check by_parties
    if (!is.logical(by_parties) | is.na(by_parties)) {

      stop(red("Ups! `by_parties` argument should be a TRUE/FALSE variable."))

    }

    # check by_parties
    if (!is.logical(short_version) | is.na(short_version)) {

      stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

    }

    # check cols_mun_var
    if (!is.null(election_data) & !all(cols_mun_var %in% names(election_data))) {

      stop(red("Ups! Variables names in `cols_mun_var` should be matched if election_data is provided."))

    }

    # check abbrev_candidacies
    # Check id_candidacies
    if (!is.null(candidacies_data) &
        !all(c(col_abbrev_candidacies, col_id_candidacies) %in% names(candidacies_data))) {

      stop(red("Ups! Variables names in `col_abbrev_candidacies` and `col_id_candidacies` should be matched with column names in candidacies_data"))

    }

    # check id_candidacies
    if (!is.null(election_data) &
        !all(col_id_candidacies %in% names(election_data))) {

      stop(red("Ups! Variables names in `col_id_candidacies` should be matched if election_data is provided."))

    }

    if (verbose) {

      message(magenta("   ... please wait, the volume of data to be aggregated may take a few seconds"))

    }

    election_data <-
      get_election_data(type_elec = type_elec, year = year, date = date,
                        election_data = election_data, ballots_data = ballots_data,
                        col_id_elec = col_id_elec, col_id_poll_station = col_id_poll_station,
                        prec_round = prec_round, short_version = FALSE,
                        verbose = verbose, lazy_duckdb = TRUE) |>
      rename_with(
        ~ c("pop_res_mun", "census_counting_mun")[match(.x, cols_mun_var)],
        .cols = cols_mun_var)
    # names(election_data)[names(election_data) %in% cols_mun_var] <-
    #   c("pop_res_mun", "census_counting_mun")

    if (verbose) {

      message(blue(glue("[x] Aggregate data at {ifelse(level == 'all', 'national', level)} level ...")))
      Sys.sleep(1/20)

    }

    if (CERA_remove) {

      message(yellow("Be careful! CERA ballots have been removed. Please, fix `CERA_remove = FALSE` if you want all ballots."))

      election_data <-
        election_data |>
        filter(!str_detect(id_INE_poll_station, "-999-"))
    }

    summary_data <-
      election_data |>
      aggregate_election_data(level = level, by_parties = by_parties,
                              col_id_elec = col_id_elec,
                              col_id_poll_station = col_id_poll_station,
                              col_id_mun = col_id_mun,
                              cols_mun_var = cols_mun_var,
                              col_id_candidacies = col_id_candidacies,
                              prec_round = prec_round,
                              verbose = verbose, short_version = FALSE,
                              lazy_duckdb = TRUE)

    # remove memory
    rm(list = c("election_data"))
    gc()

    if (verbose) {

      message(bgBlack(white("[x] Join information sources and last summaries ...\n")))
      Sys.sleep(1/20)

    }

    if (by_parties) {

      # Including some summaries
      census_var <-
        colnames(summary_data)[str_detect(colnames(summary_data), "census_counting")]
      summary_data  <-
        summary_data  |>
        mutate("porc_candidacies_parties" =
                 round(100*ballots/party_ballots, prec_round),
               "porc_candidacies_valid" =
                 round(100*ballots/valid_ballots, prec_round),
               "porc_candidacies_census" =
                 round(100*ballots/.data[[census_var]], prec_round))

    }

    if (short_version) {

      summary_data  <-
        summary_data |>
        select(-any_of(c("ballots_1", "ballots_2", "n_poll_stations",
                         cols_mun_var)), -contains("pop_res"))
    }

    if (by_parties) {

      id_party <- if_else(level == "all", col_id_candidacies[["id_nat"]],
                          col_id_candidacies[["id_prov"]])

      if (is.null(con_duckdb)) {

        con <- .get_duckdb_con()

      } else {

        con <- con_duckdb

      }


      dict_parties <-
        global_dict_parties |>
        select(-color) |>
        filter(id_elec %in% unique(summary_data |> pull(col_id_elec))) |>
        distinct(.data[[col_id_elec]], .data[[id_party]], .keep_all = TRUE)
      if (!dbExistsTable(con, "dict_parties")) {

        copy_to(con, dict_parties, "dict_parties", temporary = TRUE)

      }

      if (!dbExistsTable(con, "summary_data")) {

        copy_to(con, summary_data, "summary_data", temporary = TRUE)

      }
      summary_data <-
        tbl(con, "summary_data") |>
        left_join(tbl(con, "dict_parties"),
                  by = c("id_elec" = col_id_elec, id_party),
                  suffix = c("", ".rm")) |>
        select(-contains(".rm")) |>
        relocate(col_abbrev_candidacies, .after = id_party)

      if (level == "all") {

        summary_data <-
          summary_data |>
          select(-any_of(c("name_candidacies", col_id_candidacies[["id_prov"]]))) |>
          rename(id_candidacies = col_id_candidacies[["id_nat"]],
                 name_candidacies = name_candidacies_nat) |>
          relocate(name_candidacies, .after = col_abbrev_candidacies)

      } else {

        summary_data <-
          summary_data |>
          select(-any_of(c("name_candidacies_nat", col_id_candidacies[["id_nat"]]))) |>
          relocate(name_candidacies, .after = col_abbrev_candidacies)

      }
    }

    if (short_version) {

      summary_data <-
        summary_data |>
        select(-contains("census_counting"), -contains("pop_res"),
               -contains("id_INE"),
               -any_of(c("ballots_1", "ballots_2", "n_poll_stations")))

    }

    if (!is.na(filter_porc_ballots)) {

      if (!by_parties) {

        message(yellow("Be careful! If `by_parties` is FALSE, a filtering by percentage of party ballots cannot be achieved"))

      } else {

        summary_data  <-
          summary_data  |>
          filter(porc_candidacies_valid >= filter_porc_ballots)

      }
    }

    if (!any(is.na(filter_candidacies))) {

      if (!by_parties) {

        message(yellow("Be careful! If `by_parties` is FALSE, a filtering by candidacies cannot be achieved"))

      } else {

        filter_candidacies <-
          filter_candidacies[!is.na(filter_candidacies)]
        aux <-
          summary_data  |>
          filter(str_detect(.data[[col_abbrev_candidacies]],
                            str_flatten(filter_candidacies, collapse = "|")))

        if (nrow(aux |> collect()) == 0) {

          message(yellow("Be careful! Candidacies filter achieved by `filter_candidacies` provides a empty table so `filter_candidacies` was ignored."))

        } else {

          summary_data <- aux

        }
        # remove memory
        rm(list = c("aux"))
        gc()
      }
    }

    if (!lazy_duckdb) {

      summary_data <- summary_data |> collect()
      DBI::dbDisconnect(con, shutdown = TRUE)
    }

    # output
    return(summary_data)

}


