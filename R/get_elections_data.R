
#' @title Get municipal census data
#'
#' @description Get municipal census data for a particular election at municipal
#' level. This function is a wrapper of \code{import_raw_mun_MIR_files()}
#' function for a set of elections
#'
#' @inheritParams type_to_code_election
#' @param year,month vector of years and months of elections to be considered.
#' Please be sure (see \code{dates_elections_spain}) that elections of the
#' provided type are available for the given year and month.
#'
#' @return A tibble (as many rows as municipalities for each election) with the
#' following elements
#' \item{cod_elec}{code of type of elections: \code{"01"} (referendum),
#' \code{"02"} (congress), \code{"03"} (senate), \code{"04"} (local elections),
#' \code{"06"} (cabildo - Canarian council - elections), \code{"07"}
#' (European Parlament elections)}
#' \item{type_elec}{type of election.}
#' \item{date_elec}{date of election.}
#' \item{id_INE_mun}{municipality's id build from ccaa-prov-mun codes provided
#' by INE.}
#' \item{id_MIR_mun}{municipality's id build from ccaa-prov-mun codes provided
#' by Spanish Ministry of Interior (MIR).}
#' \item{cod_INE_ccaa, cod_MIR_ccaa, ccaa}{codes and name for regions (ccaa)
#' to which it belongs municipalities.}
#' \item{cod_INE_prov, prov}{codes and name for provinces to which it belongs
#' municipalities.}
#' \item{cod_INE_mun, cd_INE_mun, mun}{code, digit control and name for
#' municipalities.}
#' \item{cod_mun_jud_district, cod_mun_prov_council}{codes of judicial
#' district and province council.}
#' \item{n_poll_stations}{number of poll stations at each mun}
#' \item{pop_res_mun}{census of people who are living (CER + CERA).}
#' \item{census_INE_mun}{people from \code{pop_res_mun} who are
#' allowed to vote.}
#' \item{census_counting_mun}{people from \code{census_INE_mun} after claims.}
#' \item{census_CERE_mun}{ensus of foreigners, just for EU elections.}
#'
#' @author Javier Álvarez-Liébana.
#' @source Some definitions of variables were extracted from
#' \url{https://www.ige.gal}
#' @keywords get_elections_data
#' @name get_mun_census_data
#'
#' @examples
#'
#' ## Get mun census data
#'
#' # Right examples
#' mun_census_data <- get_mun_census_data("congress", 2019, 4)
#' mun_census_data <- get_mun_census_data("senate", 2019, 11)
#' mun_census_data <- get_mun_census_data(rep("congress", 3),
#'                                        c(2019, 2019, 2016),
#'                                        c(11, 4, 6))
#' mun_census_data <- get_mun_census_data(c("congress", "senate"),
#'                                        c(2019, 2019), c(11, 4))
#' \dontrun{
#' # Wrong examples
#' mun_census_data <- get_mun_census_data("national", 2019, 4)
#' mun_census_data <- get_mun_census_data("congress", 2016, c(4, 11))
#' mun_census_data <- get_mun_census_data("congress", "2016-06-26")
#' }
#'
#' @export
get_mun_census_data <-
  function(type_elec, year, month) {

    # Check: if elections required are allowed
    elections_allowed <-
      dates_elections_spain |>
      filter(year >= 1986) |>
      inner_join(tibble(cod_elec = type_to_code_election(type_elec),
                        type_elec, year, month),
                 by = c("cod_elec", "type_elec", "year", "month"))
    join_result <- elections_allowed |> nrow()
    if (join_result == 0) {

      stop(glue("No {type_elec} elections are available in {char_month}-{year}"))

    }

    # Collect raw data
    mun_data <-
      historical_raw_mun_data |>
      filter(type_elec %in% type_elec &
               year(date_elec) %in% year &
               month(date_elec) %in% month)

    # Join MIR and INE information
    mun_data <-
      mun_data |>
      left_join(cod_INE_mun,
                by = c("id_MIR_mun", "cod_MIR_ccaa",
                       "cod_INE_prov", "cod_INE_mun"),
                suffix = c(".x", "")) |>
      # Keep names from cod INE files instead of MIR files
      select(-mun.x) |>
      # Relocate
      relocate(cod_INE_ccaa, .before = cod_MIR_ccaa) |>
      relocate(id_INE_mun, .before = id_MIR_mun) |>
      relocate(cd_INE_mun, mun, .after = cod_INE_mun) |>
      relocate(ccaa, .after = cod_MIR_ccaa) |>
      relocate(prov, .after = cod_INE_prov)

    # output
    return(mun_data)
}


#' @title Get poll station data
#'
#' @description ...
#'
#' @inheritParams get_mun_census_data
#' @param prec_round rounding accuracy. Defaults to \code{prec_round = 3}.
#'
#' @return ...
#'
#' @author Javier Álvarez-Liébana.
#' @source ...
#' @keywords get_elections_data
#' @name get_poll_station_data
#'
#' @examples
#'
#' ## Get poll station data
#'
#' # Right examples
#' \dontrun{
#' # Wrong examples
#' }
#'
#' @export
get_poll_station_data <-
  function(type_elec, year, month, prec_round = 3) {

    # Check: if elections required are allowed
    elections_allowed <-
      dates_elections_spain |>
      filter(year >= 1986) |>
      inner_join(tibble(cod_elec = type_to_code_election(type_elec),
                        type_elec, year, month),
                 by = c("cod_elec", "type_elec", "year", "month"))
    join_result <- elections_allowed |> nrow()

    if (join_result == 0) {

      stop("No elections on provided dates are available")

    }

    # Check: if prec_round is a positive number
    if (prec_round != as.integer(prec_round) | prec_round < 1) {

      stop("Parameter 'prec_round' must be a positive integer greater than 0")

    }

    # Set of urls
    url_raw_data <- "https://raw.githubusercontent.com/dadosdelaplace/pollspain/remove-import-raw/data/csv/pollstation"
    urls <- glue("{url_raw_data}/raw_poll_station_data_{elections_allowed$type_elec}_{elections_allowed$year}_{elections_allowed$month}.csv")

    # Collect raw data
    poll_stations_file <-
      urls |>
      map_dfr(function(x) { read_csv(file = x, show_col_types = FALSE) }) |>
      # Some basic statistics
      mutate(valid_ballots = blank_ballots + party_ballots,
             total_ballots = valid_ballots + invalid_ballots)

    poll_station_data <-
      # First without CERA voters
      poll_stations_file |>
      filter(cod_INE_mun != "999") |>
      # Include census data of municipalities
      left_join(get_mun_census_data(type_elec, year, month),
                by = c("cod_elec", "type_elec", "date_elec", "id_MIR_mun"),
                suffix = c("", ".y")) |>
      select(-contains(".y")) |>
      # Relocate
      relocate(id_INE_mun, .before = id_MIR_mun) |>
      relocate(cod_INE_ccaa, .before = cod_MIR_ccaa) |>
      relocate(ccaa, .after = cod_MIR_ccaa) |>
      relocate(prov, .after = cod_INE_prov) |>
      relocate(cd_INE_mun, mun, .after = cod_INE_mun) |>
      # Remove census variables at mun level from mun files
      select(-c(census_counting_mun, census_CERE_mun, census_INE_mun)) |>
      # Include CERA "municipalities" and their ccaa and prov
      bind_rows(poll_stations_file |> filter(cod_INE_mun == "999")) |>
      left_join(cod_INE_mun |>
                  distinct(cod_MIR_ccaa, cod_INE_prov, .keep_all = TRUE) |>
                  select(contains("ccaa") | contains("prov")),
                by = c("cod_MIR_ccaa", "cod_INE_prov"),
                suffix = c("", ".y")) |>
      mutate(cod_INE_ccaa =
               ifelse(is.na(cod_INE_ccaa), cod_INE_ccaa.y, cod_INE_ccaa),
             ccaa = ifelse(is.na(ccaa), ccaa.y, ccaa),
             prov = ifelse(is.na(prov), prov.y, prov),
             mun = ifelse(cod_INE_mun == "999", "CERA", mun),
             id_INE_mun = glue("{cod_INE_ccaa}-{cod_INE_prov}-{cod_INE_mun}"),
             pop_res_mun =
               ifelse(cod_INE_mun == "999", census_INE, pop_res_mun)) |>
      # Remove variables
      select(-contains(".y"), -cod_MIR_ccaa)

    # Include turnout data (use census_counting from poll stations files)
    poll_station_data <-
      poll_station_data |>
      drop_na(id_INE_mun) |>
      mutate(id_INE_poll_station =
               glue("{id_INE_mun}-{cod_mun_district}-{cod_sec}-{cod_poll_station}"),
             turnout_1 = round(100 * ballots_1 / census_counting, prec_round),
             turnout_2 = round(100 * ballots_2 / census_counting, prec_round),
             turnout = round(100 * total_ballots / census_counting, prec_round),
             turnout_abs = 100 - turnout,
             # % valid and invalid ballots over total ballots
             porc_valid =
               round(100 * valid_ballots / total_ballots, prec_round),
             porc_invalid =
               round(100 * invalid_ballots / total_ballots, prec_round),
             # % party and blank ballots over valid ballots
             porc_parties =
               round(100 * party_ballots / valid_ballots, prec_round),
             porc_blank =
               round(100 * blank_ballots / valid_ballots, prec_round)) |>
      relocate(turnout:porc_blank, .after = total_ballots) |>
      relocate(id_INE_poll_station, .after = date_elec) |>
      relocate(turnout_1, .after = ballots_1) |>
      relocate(turnout_2, .after = ballots_2)

    # Select just few variables
    poll_station_data <-
      poll_station_data |>
      mutate(id_INE_poll_station =
               glue("{id_INE_mun}-{cod_mun_district}-{cod_sec}-{cod_poll_station}"),
             id_elec = glue("{type_to_code_election(type_elec)}-{date_elec}")) |>
      select(id_elec, type_elec, date_elec, id_INE_poll_station, ccaa, prov, mun,
             census_counting, ballots_1, turnout_1, ballots_2, turnout_2,
             blank_ballots, invalid_ballots, party_ballots, valid_ballots,
             total_ballots, turnout, porc_valid, porc_invalid,
             porc_parties, porc_blank, pop_res_mun)

    # output
    return(poll_station_data)
}


#' @title Get candidates data
#'
#' @description ...
#'
#' @inheritParams get_mun_census_data
#'
#' @return ...
#'
#' @author Javier Álvarez-Liébana.
#' @source ...
#' @keywords get_elections_data
#' @name get_candidates_data
#'
#' @examples
#'
#' ## Get candidates data
#'
#' # Right examples
#' \dontrun{
#' # Wrong examples
#' }
#'
#' @export
get_candidates_data <-
  function(type_elec, year, month) {

    # Check: if elections required are allowed
    elections_allowed <-
      dates_elections_spain |>
      filter(year >= 1986) |>
      inner_join(tibble(cod_elec = type_to_code_election(type_elec),
                        type_elec, year, month),
                 by = c("cod_elec", "type_elec", "year", "month"))
    join_result <- elections_allowed |> nrow()
    if (join_result == 0) {

      stop("No elections on provided dates are available")

    }

    # Collect candidates data
    candidates_data <-
      historical_raw_candidates |>
      filter(type_elec %in% type_elec &
               year(date_elec) %in% year &
               month(date_elec) %in% month) |>
      # cod_mun_district = "9" when elections have not that circumscription
      # cod_INE_mun just for municipalities' elections or
      #  Senate's elections (senator's code). Otherwise, cod_INE_mun = "999"
      mutate(cod_mun_district =
               ifelse(cod_mun_district == "9", NA, cod_mun_district),
             cod_INE_mun =
               ifelse(cod_INE_mun == "999", NA, cod_INE_mun))

    # Output
    return(candidates_data)
}


#' @title Get candidacies data (at poll station level)
#'
#' @description ...
#'
#' @inheritParams get_mun_census_data
#' @param include_candidates flag to indicate wheter it should be included data
#' about candidates or not. Defaults to \code{FALSE}.
#'
#' @return ...
#'
#' @author Javier Álvarez-Liébana.
#' @source ...
#' @keywords get_elections_data
#' @name get_candidacies_data
#'
#' @examples
#'
#' ## Get candidacies data
#'
#' # Right examples
#' \dontrun{
#' # Wrong examples
#' }
#'
#' @export
get_candidacies_data <-
  function(type_elec, year, month, include_candidates = FALSE) {

    # Check: if elections required are allowed
    elections_allowed <-
      dates_elections_spain |>
      filter(year >= 1986) |>
      inner_join(tibble(cod_elec = type_to_code_election(type_elec),
                        type_elec, year, month),
                 by = c("cod_elec", "type_elec", "year", "month"))
    join_result <- elections_allowed |> nrow()
    if (join_result == 0) {

      stop("No elections on provided dates are available")

    }

    # Check: if include_candidates should be logical
    if (!is.logical(include_candidates)) {

      stop("Parameter 'include_candidates' must be a logical variable")

    }

    # Set of urls
    url_raw_data <- "https://raw.githubusercontent.com/dadosdelaplace/pollspain/remove-import-raw/data/csv/candidacies_pollstation"
    urls <- glue("{url_raw_data}/raw_candidacies_poll_{elections_allowed$type_elec}_{elections_allowed$year}_{elections_allowed$month}.csv")

    # Collect raw data
    candidacies_files <-
      urls |>
      map_dfr(function(x) { read_csv(file = x, show_col_types = FALSE) })

    # Join MIR and INE information
    candidacies_ballots <-
      candidacies_files |>
      left_join(cod_INE_mun |> # First include CERA codes
                  bind_rows(cod_INE_mun |>
                              distinct(cod_INE_ccaa, cod_INE_prov,
                                       .keep_all  = TRUE) |>
                              select(cod_INE_ccaa, cod_MIR_ccaa,
                                     cod_INE_prov, ccaa, prov) |>
                              mutate(mun = "CERA", cod_INE_mun = "999",
                                     id_INE_mun =
                                       glue("{cod_INE_ccaa}-{cod_INE_prov}-{cod_INE_mun}"),
                                     id_MIR_mun =
                                       glue("{cod_MIR_ccaa}-{cod_INE_prov}-{cod_INE_mun}"))),
                by = "id_MIR_mun",
                suffix = c(".x", "")) |>
      # Keep names from cod INE files instead of MIR files
      select(-contains(".x")) |>
      # Include id_INE_poll_station
      mutate(id_INE_poll_station =
               glue("{id_INE_mun}-{cod_mun_district}-{cod_sec}-{cod_poll_station}")) |>
      # Relocate
      relocate(id_INE_poll_station, .after = date_elec) |>
      relocate(id_INE_mun, .before = id_MIR_mun) |>
      relocate(cod_INE_ccaa, cod_MIR_ccaa, ccaa,
               cod_INE_prov, prov, cod_INE_mun,
               cd_INE_mun, mun, .after = id_MIR_mun)

    # Join candidacies data
    candidacies_data <-
      candidacies_ballots |>
      left_join(historical_raw_candidacies |>
                  filter(type_elec %in% type_elec &
                           year(date_elec) %in% year &
                           month(date_elec) %in% month),
                by = c("cod_elec", "type_elec",
                       "date_elec", "id_candidacies"))


    # include candidates
    if (include_candidates) {

      # Collect candidates files
      candidates_files <- get_candidates_data(type_elec, year, month)

      # Include candidates data
      candidacies_data <-
        candidacies_data |>
        left_join(candidates_files |>
                    select(-cod_mun_district, -cod_INE_mun),
                  by = c("cod_elec", "type_elec", "cod_INE_prov",
                         "date_elec", "id_candidacies", "turn"))

      # Rename
      candidacies_data <-
        candidacies_data |>
        select(-id_candidacies) |>
        rename(candidate_name = name, candidate_surname = surname,
               candidate_order = order, candidate_holder = holder,
               candidate_sex = sex, candidate_elected = elected,
               id_candidacies = cod_candidacies_nat,
               id_candidacies_prov = cod_candidacies_prov) |>
        select(type_elec, date_elec, id_INE_poll_station, ccaa, prov,
               mun, id_candidacies, id_candidacies_prov, abbrev_candidacies,
               name_candidacies,  ballots, candidate_name, candidate_surname,
               candidate_order, candidate_holder, candidate_sex,
               candidate_elected)

    } else {

      # Without candidates files but number of elected

      # Collect candidates files
      candidates_files <- get_candidates_data(type_elec, year, month)

      # Compute number of elected
      candidates_files <-
        candidates_files |>
        filter(elected) |>
        select(cod_elec:cod_INE_prov, id_candidacies, elected) |>
        reframe(elected_by_prov = sum(elected),
                .by = c(cod_elec, type_elec, date_elec,
                       cod_INE_prov, id_candidacies))

      # join information
      candidacies_data <-
        candidacies_data |>
        left_join(candidates_files,
                  by = c("cod_elec", "type_elec", "date_elec",
                         "cod_INE_prov", "id_candidacies")) |>
        mutate(elected_by_prov =
                 ifelse(is.na(elected_by_prov), 0, elected_by_prov))

      # Select a few variables
      candidacies_data <-
        candidacies_data |>
        # just id candidacies as cod_candidacies_nat
        select(-id_candidacies) |>
        rename(id_candidacies = cod_candidacies_nat,
               id_candidacies_prov = cod_candidacies_prov) |>
        select(type_elec, date_elec, id_INE_poll_station, ccaa, prov,
               mun, id_candidacies, id_candidacies_prov, abbrev_candidacies,
               name_candidacies,  ballots, elected_by_prov)

    }

    # Recoding parties
    candidacies_data <-
      candidacies_data |> recod_parties()

    # output
    return(candidacies_data)
  }


#' @title Get elections data
#'
#' @description ...
#'
#' @inheritParams get_candidacies_data
#' @inheritParams get_poll_station_data
#' @param include_candidacies flag to indicate wheter it should be included data
#' about candidacies or not. Defaults to \code{FALSE}.
#'
#' @return ...
#'
#' @author Javier Álvarez-Liébana.
#' @source ...
#' @keywords get_elections_data
#' @name get_elections_data
#'
#' @examples
#'
#' ## Get whole elections data
#'
#' # Right examples
#' \dontrun{
#' # Wrong examples
#' }
#'
#' @export
get_elections_data <-
  function(type_elec, year, month, prec_round = 3,
           include_candidacies = FALSE,
           include_candidates = FALSE) {

    # Code of election
    cod_elec <-
      type_elec |>
      map_chr(function(x) {
        type_to_code_election(x)
        })

    # Check: if elections required are allowed
    join_result <-
      dates_elections_spain |>
      inner_join(tibble(cod_elec, year, month),
                 by = c("cod_elec", "year", "month")) |>
      nrow()
    if (join_result == 0) {

      stop("No elections on provided dates are available")

    }

    # Check: if prec_round is a positive number
    if (prec_round != as.integer(prec_round) | prec_round < 1) {

      stop("Parameter 'prec_round' must be a positive integer greater than 0")

    }

    # Check: if include_candidates should be logical
    if (!is.logical(include_candidates) | !is.logical(include_candidacies)) {

      stop("Parameter 'include_candidates' and 'include_candidacies' must be logical variables")

    }

    # Getting data at poll station level (without candidacies)
    election_data <-
      get_poll_station_data(type_elec, year, month,
                            prec_round = prec_round)

    if (include_candidacies) {

      # Getting candidacies data
      candidacies_data <-
        get_candidacies_data(type_elec, year, month,
                             include_candidates = include_candidates)

      # Join information
      election_data <-
        election_data |>
        left_join(candidacies_data,
                  by = c("type_elec", "date_elec",
                         "id_INE_poll_station"),
                  suffix = c("", ".y"), multiple = "all") |>
        select(-contains(".y")) |>
        recod_parties()

    }

    # output
    return(election_data)

}



#' @title Aggregate elections data
#'
#' @description ...
#'
#' @param election_data tibble containing election data
#' @param level level of territorial aggregation. Values allowed are "all"
#' (national aggregation), "ccaa" (regional aggregation),
#' "prov" (province aggregation), "mun" (municipal aggregation),
#' "mun-district" (municipal district aggregation),
#' "sec" (census section aggregation) or "poll-station" (without aggregation)
#' @param by_parties flag to indicate wheter it should be aggregate data
#' about ballots of parties (as long as \code{election_data} has been generated
#' with the candidacies data). Defaults to \code{TRUE}.
#' @param prec_round rounding accuracy. Defaults to \code{prec_round = 3}.
#'
#' @return ...
#'
#' @author Javier Álvarez-Liébana.
#' @source ...
#' @keywords get_elections_data
#' @name aggregate_election_data
#'
#' @examples
#'
#' ## Aggregate election data
#'
#' # Right examples
#' \dontrun{
#' # Wrong examples
#' }
#'
#' @export
aggregate_election_data <-
  function(election_data, level = "all",
           by_parties = TRUE, prec_round = 3) {

    # Check: if prec_round is a positive number
    if (prec_round != as.integer(prec_round) | prec_round < 1) {

      stop("Parameter 'prec_round' must be a positive integer greater than 0")

    }

    # Check: if level takes allowed values
    if (!(level %in% c("all", "ccaa", "prov", "mun",
                       "mun-district", "sec", "poll-station"))) {

      stop("Aggregation level provided by 'level' parameter should be taken from the following values: 'all', 'ccaa', 'prov', 'mun', 'mun-district', 'sec', 'poll-station'")

    }

    # Check: if by_parties is a logical variable
    if (!is.logical(by_parties)) {

      stop(glue("Parameter 'by_parties' must be a logical variable, just TRUE/FALSE are avoided"))

    }

    # Remove duplicate rows (candidates), just one
    # for type, date, poll station and party
    if ("id_candidacies" %in% names(election_data)) {

      election_data <-
        election_data |>
        distinct(id_elec, type_elec, date_elec, id_INE_poll_station,
                 id_candidacies, .keep_all = TRUE)

    } else {

      election_data <-
        election_data |>
        distinct(id_elec, type_elec, date_elec, id_INE_poll_station,
                 .keep_all = TRUE)
    }

    # Aggregation level: national
    if (level == "all") {

      # CERA summaries
      data_cera <-
        election_data |>
        filter(extract_code(id_INE_poll_station, level = "mun") == "999") |>
        group_by(id_elec, type_elec, date_elec) |>
        distinct(id_INE_poll_station, .keep_all = TRUE) |>
        reframe(census_cera = sum(census_counting),
                total_ballots_cera = sum(total_ballots),
                turnout_cera =
                  round(100 * total_ballots_cera / census_cera, prec_round)) |>
        ungroup()

      # Resident population (without CERA)
      pop_res <-
        election_data |>
        # Saving time
        distinct(id_elec, type_elec, date_elec, id_INE_poll_station,
                 .keep_all = TRUE) |>
        mutate(id_INE_mun =
                 extract_code(id_INE_poll_station, level = "mun",
                              full_cod = TRUE)) |>
        filter(!str_detect(id_INE_mun, "-999")) |>
        distinct(id_elec, type_elec, date_elec, id_INE_mun,
                 .keep_all = TRUE) |>
        summarise(pop_res = sum(pop_res_mun),
                  .by = c(id_elec, type_elec, date_elec))

      # Aggregation of data
      agg_data <-
        election_data |>
        # Ignore multiple rows due to candidacies
        distinct(id_elec, type_elec, date_elec, id_INE_poll_station,
                 .keep_all = TRUE) |>
        # Summaries by type of election and date
        reframe(# npoll stations without cera
                n_poll_stations = n_distinct(id_INE_poll_station) - 52,
                across(c("census_counting", "ballots_1",
                         "ballots_2", "blank_ballots":"total_ballots"), sum),
                turnout =
                  round(100 * total_ballots / census_counting, prec_round),
                porc_valid =
                  round(100 * valid_ballots / total_ballots, prec_round),
                porc_invalid =
                  round(100 * invalid_ballots / total_ballots, prec_round),
                porc_parties =
                  round(100 * party_ballots / valid_ballots, prec_round),
                porc_blank =
                  round(100 * blank_ballots / valid_ballots, prec_round),
                .by = c(id_elec, type_elec, date_elec)) |>
        # Join CERA data
        left_join(data_cera, by = c("id_elec", "type_elec", "date_elec")) |>
        # turnout_1 and turnout_2 over counting census without cera.
        mutate(turnout_1 =
                 round(100 * ballots_1 / (census_counting - census_cera),
                       prec_round),
               turnout_2 =
                 round(100 * ballots_2 / (census_counting - census_cera),
                       prec_round)) |>
        # Join pop res data
        left_join(pop_res, by = c("id_elec", "type_elec", "date_elec")) |>
        # Relocate
        relocate(pop_res, .after = date_elec) |>
        relocate(turnout_1, .after = ballots_1) |>
        relocate(turnout_2, .after = ballots_2)

      # Aggregation of parties' data
      if (by_parties) {

        # Use as name the "variant" of party most voted
        most_party_data <-
          election_data |>
          group_by(id_elec, type_elec, date_elec, id_candidacies,
                   abbrev_candidacies, name_candidacies) |>
          summarise(ballots = sum(ballots)) |>
          ungroup(abbrev_candidacies, name_candidacies) |>
          slice_max(ballots, n = 1) |>
          ungroup() |>
          select(-ballots)

        # Compute number of elected by election, province and parties
        elected <-
          election_data |>
          distinct(id_elec, type_elec, date_elec, prov, id_candidacies,
                   .keep_all = TRUE) |>
          summarise(elected = sum(elected_by_prov),
                    .by = c(id_elec, type_elec, date_elec, id_candidacies))

        # Aggregation of parties' data
        agg_party_data <-
          election_data |>
          summarise(ballots = sum(ballots),
                    .by = c(id_elec, type_elec, date_elec, id_candidacies)) |>
          # Name of party
          left_join(most_party_data,
                    by = c("id_elec", "type_elec", "date_elec", "id_candidacies"),
                    suffix = c(".x", ""), multiple = "all") |>
          select(-contains(".x")) |>
          # Number of elected
          left_join(elected, by = c("id_elec", "type_elec",
                                    "date_elec", "id_candidacies"))

        # Join parties data
        agg_data <-
          agg_data |>
          left_join(agg_party_data, by = c("id_elec", "type_elec", "date_elec"),
                    multiple = "all") |>
          # Some summaries
          # Votes required for each elected
          mutate(votes_by_elected =
                   ifelse(elected > 0, round(ballots / elected, 0),
                          NA),
                 .by = c(type_elec, date_elec, id_candidacies)) |>
          # Summaries
          mutate(porc_parties_valid =
                   round(100 * ballots / valid_ballots, prec_round),
                 porc_parties_census =
                   round(100 * ballots / census_counting, prec_round),
                 porc_parties_cand =
                   round(100 * ballots / party_ballots, prec_round)) |>
          # Relocate
          relocate(ballots, .after = name_candidacies)

      }

      # output
      return(agg_data)
    }

    # Aggregation level: ccaa
    if (level == "ccaa") {

      # If cod_INE_ccaa is not include, id is created
      if (!("cod_INE_ccaa" %in% names(election_data))) {

        election_data <-
          election_data |>
          mutate(cod_INE_ccaa =
                   extract_code(id_INE_poll_station, level = "ccaa"))

      }

      # CERA summaries
      data_cera <-
        election_data |>
        filter(extract_code(id_INE_poll_station, level = "mun") == "999") |>
        group_by(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa) |>
        distinct(id_INE_poll_station, .keep_all = TRUE) |>
        summarise(census_cera = sum(census_counting),
                  total_ballots_cera = sum(total_ballots),
                  turnout_cera =
                    round(100 * total_ballots_cera / census_cera, prec_round)) |>
        ungroup()

      # Resident population (without CERA)
      pop_res <-
        election_data |>
        mutate(id_INE_mun =
                 extract_code(id_INE_poll_station, level = "mun",
                              full_cod = TRUE)) |>
        filter(!str_detect(id_INE_mun, "-999")) |>
        distinct(id_elec, type_elec, date_elec, id_INE_mun, .keep_all = TRUE) |>
        summarise(pop_res = sum(pop_res_mun),
                  .by = c(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa))

      # Aggregation of data
      agg_data <-
        election_data |>
        # Ignore multiple rows due to candidacies
        distinct(id_elec, type_elec, date_elec, id_INE_poll_station,
                 .keep_all = TRUE) |>
        reframe(n_poll_stations = n() - length(unique(prov)),
                across(c("census_counting", "ballots_1",
                         "ballots_2", "blank_ballots":"total_ballots"), sum),
                turnout =
                  round(100 * total_ballots / census_counting, prec_round),
                porc_valid =
                  round(100 * valid_ballots / total_ballots, prec_round),
                porc_invalid =
                  round(100 * invalid_ballots / total_ballots, prec_round),
                porc_parties =
                  round(100 * party_ballots / valid_ballots, prec_round),
                porc_blank =
                  round(100 * blank_ballots / valid_ballots, prec_round),
                .by = c(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa)) |>
        # Join CERA data
        left_join(data_cera,
                  by = c("id_elec", "type_elec", "date_elec",
                         "cod_INE_ccaa", "ccaa")) |>
        # turnout_1 and turnout_2 over counting census without cera.
        mutate(turnout_1 =
                 round(100 * ballots_1 / (census_counting - census_cera),
                       prec_round),
               turnout_2 =
                 round(100 * ballots_2 / (census_counting - census_cera),
                       prec_round)) |>
        # Join pop res data
        left_join(pop_res,
                  by = c("id_elec", "type_elec", "date_elec",
                         "cod_INE_ccaa", "ccaa")) |>
        # Relocate
        relocate(pop_res, .after = ccaa) |>
        relocate(turnout_1, .after = ballots_1) |>
        relocate(turnout_2, .after = ballots_2)


      # Aggregation of parties' data
      if (by_parties) {

        # Use as name the "variant" of party most voted
        # at each ccaa
        most_party_data <-
          election_data |>
          group_by(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                   id_candidacies, abbrev_candidacies, name_candidacies) |>
          summarise(ballots = sum(ballots)) |>
          ungroup(abbrev_candidacies, name_candidacies) |>
          slice_max(ballots, n = 1) |>
          ungroup() |>
          select(-ballots)

        # Compute number of elected
        elected <-
          election_data |>
          distinct(id_elec, type_elec, date_elec, prov, id_candidacies,
                   .keep_all = TRUE) |>
          reframe(elected = sum(elected_by_prov),
                  .by = c(id_elec, type_elec, date_elec, cod_INE_ccaa,
                          ccaa, id_candidacies))

        # Aggregation of parties' data
        agg_party_data <-
          election_data |>
          reframe(ballots = sum(ballots),
                  .by = c(id_elec, type_elec, date_elec,
                          cod_INE_ccaa, ccaa, id_candidacies)) |>
          # Name of party
          left_join(most_party_data,
                    by = c("id_elec", "type_elec", "date_elec", "cod_INE_ccaa",
                           "ccaa", "id_candidacies"),
                    suffix = c(".x", "")) |>
          select(-contains(".x")) |>
          # Number of elected
          left_join(elected,
                    by = c("id_elec", "type_elec", "date_elec", "cod_INE_ccaa",
                           "ccaa", "id_candidacies"))

        # Join parties data
        agg_data <-
          agg_data |>
          left_join(agg_party_data,
                    by = c("id_elec", "type_elec", "date_elec",
                           "cod_INE_ccaa", "ccaa")) |>
          # Some summaries
          # Votes required for each elected
          mutate(votes_by_elected =
                   ifelse(elected > 0, round(ballots / elected, 0),
                          NA),
                 .by = c(id_elec, type_elec, date_elec, cod_INE_ccaa,
                         ccaa, id_candidacies)) |>
          # Summaries
          mutate(porc_parties_valid =
                   round(100 * ballots / valid_ballots, prec_round),
                 porc_parties_census =
                   round(100 * ballots / census_counting, prec_round),
                 porc_parties_cand =
                   round(100 * ballots / party_ballots, prec_round)) |>
          # Relocate
          relocate(ballots, .after = name_candidacies)

      }

      # output
      return(agg_data)

    }

    # Aggregation level: prov
    if (level == "prov") {

      # If cod_INE_prov is not include, id is created
      if (!("cod_INE_prov" %in% names(election_data))) {

        election_data <-
          election_data |>
          mutate(cod_INE_ccaa =
                   extract_code(id_INE_poll_station, level = "ccaa"),
                 cod_INE_prov =
                   extract_code(id_INE_poll_station, level = "prov"))

      }


      # CERA summaries
      data_cera <-
        election_data |>
        filter(extract_code(id_INE_poll_station, level = "mun") == "999") |>
        group_by(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                 cod_INE_prov, prov) |>
        distinct(id_INE_poll_station, .keep_all = TRUE) |>
        reframe(census_cera = sum(census_counting),
                total_ballots_cera = sum(total_ballots),
                turnout_cera =
                  round(100 * total_ballots_cera / census_cera, prec_round)) |>
        ungroup()

      # Resident population (without CERA)
      pop_res <-
        election_data |>
        mutate(id_INE_mun =
                 extract_code(id_INE_poll_station, level = "mun",
                              full_cod = TRUE)) |>
        filter(!str_detect(id_INE_mun, "-999")) |>
        distinct(id_elec, type_elec, date_elec, id_INE_mun, .keep_all = TRUE) |>
        summarise(pop_res = sum(pop_res_mun),
                  .by = c(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                          cod_INE_prov, prov))

      # Aggregation of data
      agg_data <-
        election_data |>
        # Ignore multiple rows due to candidacies
        distinct(id_elec, type_elec, date_elec, id_INE_poll_station,
                 .keep_all = TRUE) |>
        reframe(n_poll_stations = n() - 1,
                across(c("census_counting", "ballots_1",
                         "ballots_2", "blank_ballots":"total_ballots"), sum),
                turnout =
                  round(100 * total_ballots / census_counting, prec_round),
                porc_valid =
                  round(100 * valid_ballots / total_ballots, prec_round),
                porc_invalid =
                  round(100 * invalid_ballots / total_ballots, prec_round),
                porc_parties =
                  round(100 * party_ballots / valid_ballots, prec_round),
                porc_blank =
                  round(100 * blank_ballots / valid_ballots, prec_round),
                .by = c(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                        cod_INE_prov, prov)) |>
        # Join CERA data
        left_join(data_cera,
                  by = c("id_elec", "type_elec", "date_elec", "cod_INE_ccaa",
                         "ccaa", "cod_INE_prov", "prov")) |>
        # turnout_1 and turnout_2 over counting census without cera.
        mutate(turnout_1 =
                 round(100 * ballots_1 / (census_counting - census_cera),
                       prec_round),
               turnout_2 =
                 round(100 * ballots_2 / (census_counting - census_cera),
                       prec_round)) |>
        # Join pop res data
        left_join(pop_res,
                  by = c("id_elec", "type_elec", "date_elec", "cod_INE_ccaa",
                         "ccaa", "cod_INE_prov", "prov")) |>
        # Relocate
        relocate(pop_res, .after = prov) |>
        relocate(turnout_1, .after = ballots_1) |>
        relocate(turnout_2, .after = ballots_2)

      # Aggregation of parties' data
      if (by_parties) {

        # Use as name the "variant" of party most voted
        # at each ccaa
        most_party_data <-
          election_data |>
          group_by(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                   cod_INE_prov, prov, id_candidacies,
                   abbrev_candidacies, name_candidacies) |>
          summarise(ballots = sum(ballots)) |>
          ungroup(abbrev_candidacies, name_candidacies) |>
          slice_max(ballots, n = 1) |>
          ungroup() |>
          select(-ballots)

        # Compute number of elected
        elected <-
          election_data |>
          distinct(id_elec, type_elec, date_elec, prov, id_candidacies,
                   .keep_all = TRUE) |>
          reframe(elected = sum(elected_by_prov),
                  .by = c(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                          cod_INE_prov, prov, id_candidacies))

        # Aggregation of parties' data
        agg_party_data <-
          election_data |>
          summarise(ballots = sum(ballots),
                    .by = c(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                            cod_INE_prov, prov, id_candidacies)) |>
          # Name of party
          left_join(most_party_data,
                    by = c("id_elec", "type_elec", "date_elec", "cod_INE_ccaa",
                           "ccaa", "cod_INE_prov", "prov",
                           "id_candidacies"),
                    suffix = c(".x", "")) |>
          select(-contains(".x")) |>
          # Number of elected
          left_join(elected,
                    by = c("id_elec", "type_elec", "date_elec", "cod_INE_ccaa",
                           "ccaa", "cod_INE_prov", "prov", "id_candidacies"))

        # Join parties data
        agg_data <-
          agg_data |>
          left_join(agg_party_data,
                    by = c("id_elec", "type_elec", "date_elec", "cod_INE_ccaa",
                           "ccaa", "cod_INE_prov", "prov")) |>
          # Some summaries
          group_by(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                   cod_INE_prov, prov, id_candidacies) |>
          # Votes required for each elected
          mutate(votes_by_elected =
                   ifelse(elected > 0, round(ballots / elected, 0),
                          NA)) |>
          ungroup() |>
          # Summaries
          mutate(porc_parties_valid =
                   round(100 * ballots / valid_ballots, prec_round),
                 porc_parties_census =
                   round(100 * ballots / census_counting, prec_round),
                 porc_parties_cand =
                   round(100 * ballots / party_ballots, prec_round)) |>
          # Relocate
          relocate(ballots, .after = name_candidacies)

      }

      # output
      return(agg_data)

    }

    # Aggregation level: munipalities
    if (level == "mun") {

      # If cod_INE_prov is not include, id is created
      if (!("cod_INE_mun" %in% names(election_data))) {

        election_data <-
          election_data |>
          mutate(cod_INE_ccaa =
                   extract_code(id_INE_poll_station, level = "ccaa"),
                 cod_INE_prov =
                   extract_code(id_INE_poll_station, level = "prov"),
                 id_INE_mun =
                   extract_code(id_INE_poll_station, level = "mun",
                                full_cod = TRUE),
                 cod_INE_mun =
                   extract_code(id_INE_poll_station, level = "mun"))

      }

      # CERA summaries
      data_cera <-
        election_data |>
        filter(str_detect(id_INE_mun, "-999")) |>
        group_by(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                 cod_INE_prov, prov, id_INE_mun, mun) |>
        distinct(id_INE_poll_station, .keep_all = TRUE) |>
        summarise(census_cera = sum(census_counting),
                  total_ballots_cera = sum(total_ballots),
                  turnout_cera =
                    round(100 * total_ballots_cera / census_cera,
                          prec_round)) |>
        ungroup()

      # Resident population (without CERA)
      pop_res <-
        election_data |>
        filter(!str_detect(id_INE_mun, "999")) |>
        distinct(id_elec, type_elec, date_elec, id_INE_mun, .keep_all = TRUE) |>
        group_by(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                 cod_INE_prov, prov, id_INE_mun, mun) |>
        summarise(pop_res = sum(pop_res_mun)) |>
        ungroup()

      # Aggregation of data
      agg_data <-
        election_data |>
        # Ignore multiple rows due to candidacies
        distinct(id_elec, type_elec, date_elec, id_INE_poll_station,
                 .keep_all = TRUE) |>
        reframe(n_poll_stations = n(),
                across(c("census_counting", "ballots_1",
                         "ballots_2", "blank_ballots":"total_ballots"), sum),
                turnout =
                  round(100 * total_ballots / census_counting, prec_round),
                porc_valid =
                  round(100 * valid_ballots / total_ballots, prec_round),
                porc_invalid =
                  round(100 * invalid_ballots / total_ballots, prec_round),
                porc_parties =
                  round(100 * party_ballots / valid_ballots, prec_round),
                porc_blank =
                  round(100 * blank_ballots / valid_ballots, prec_round),
                .by = c(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                        cod_INE_prov, prov, id_INE_mun, mun)) |>
        # Join CERA data
        left_join(data_cera,
                  by = c("id_elec", "type_elec", "date_elec",
                         "cod_INE_ccaa", "ccaa", "cod_INE_prov", "prov",
                         "id_INE_mun", "mun")) |>
        # turnout_1 and turnout_2 for municipalities CERA will be NA
        mutate(turnout_1 =
                 round(100 * ballots_1 / (census_counting - census_cera),
                       prec_round),
               turnout_1 = ifelse(is.nan(turnout_1), NA, turnout_1),
               turnout_2 =
                 round(100 * ballots_2 / (census_counting - census_cera),
                       prec_round),
               turnout_2 = ifelse(is.nan(turnout_2), NA, turnout_2)) |>
        # Join pop res data
        left_join(pop_res,
                  by = c("id_elec", "type_elec", "date_elec",
                         "cod_INE_ccaa", "ccaa", "cod_INE_prov", "prov",
                         "id_INE_mun", "mun")) |>
        # Relocate
        relocate(pop_res, .after = mun) |>
        relocate(turnout_1, .after = ballots_1) |>
        relocate(turnout_2, .after = ballots_2) |>
        # Remove CERA rows
        mutate(n_poll_stations = ifelse(mun == "CERA", NA, n_poll_stations))

      if (by_parties == TRUE) {

        warning("A summary at municipality level cannot be done with party ballots for congress elections")

      }

      # output
      return(agg_data)

    }

    # Aggregation level: electoral district
    if (level == "mun-district") {

      # If cod_INE_prov is not include, id is created
      if (!("cod_mun_district" %in% names(election_data))) {

        election_data <-
          election_data |>
          mutate(cod_INE_ccaa =
                   extract_code(id_INE_poll_station, level = "ccaa"),
                 cod_INE_prov =
                   extract_code(id_INE_poll_station, level = "prov"),
                 id_INE_mun =
                   extract_code(id_INE_poll_station, level = "mun",
                                full_cod = TRUE),
                 cod_mun_district =
                   extract_code(id_INE_poll_station, level = "mun-district"))

      }

      # CERA summaries
      data_cera <-
        election_data |>
        filter(str_detect(id_INE_mun, "999")) |>
        distinct(id_elec, id_INE_poll_station, .keep_all = TRUE) |>
        group_by(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                 cod_INE_prov, prov, id_INE_mun, mun) |>
        summarise(census_cera = sum(census_counting),
                  total_ballots_cera = sum(total_ballots),
                  turnout_cera =
                    round(100 * total_ballots_cera / census_cera,
                          prec_round)) |>
        ungroup()

      # Resident population (without CERA)
      pop_res <-
        election_data |>
        filter(!str_detect(id_INE_mun, "999")) |>
        distinct(id_elec, type_elec, date_elec, id_INE_mun, .keep_all = TRUE) |>
        group_by(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                 cod_INE_prov, prov, id_INE_mun, mun) |>
        summarise(pop_res_mun = sum(pop_res_mun)) |>
        ungroup()

      # Aggregation of data
      agg_data <-
        election_data |>
        # Ignore multiple rows due to candidacies
        distinct(id_elec, type_elec, date_elec, id_INE_poll_station,
                 .keep_all = TRUE) |>
        reframe(n_poll_stations = n(),
                  across(c("census_counting", "ballots_1",
                           "ballots_2", "blank_ballots":"total_ballots"), sum),
                  turnout =
                    round(100 * total_ballots / census_counting, prec_round),
                  porc_valid =
                    round(100 * valid_ballots / total_ballots, prec_round),
                  porc_invalid =
                    round(100 * invalid_ballots / total_ballots, prec_round),
                  porc_parties =
                    round(100 * party_ballots / valid_ballots, prec_round),
                  porc_blank =
                    round(100 * blank_ballots / valid_ballots, prec_round),
                .by = c(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                        cod_INE_prov, prov, id_INE_mun, mun,
                        cod_mun_district)) |>
        # Join CERA data
        left_join(data_cera,
                  by = c("id_elec", "type_elec", "date_elec", "cod_INE_ccaa",
                         "ccaa", "cod_INE_prov", "prov", "id_INE_mun",
                         "mun")) |>
        # turnout_1 and turnout_2 for municipalities CERA will be NA
        mutate(turnout_1 =
                 round(100 * ballots_1 / (census_counting - census_cera),
                       prec_round),
               turnout_1 = ifelse(is.nan(turnout_1), NA, turnout_1),
               turnout_2 =
                 round(100 * ballots_2 / (census_counting - census_cera),
                       prec_round),
               turnout_2 = ifelse(is.nan(turnout_2), NA, turnout_2)) |>
        # Join pop res data
        left_join(pop_res,
                  by = c("id_elec", "type_elec", "date_elec", "cod_INE_ccaa",
                         "ccaa", "cod_INE_prov", "prov", "id_INE_mun",
                         "mun")) |>
        # Relocate
        relocate(pop_res_mun, .after = mun) |>
        relocate(turnout_1, .after = ballots_1) |>
        relocate(turnout_2, .after = ballots_2) |>
        # Remove CERA rows
        mutate(n_poll_stations = ifelse(mun == "CERA", NA, n_poll_stations))

      if (by_parties == TRUE) {

        warning("A summary at district level cannot be done with party ballots for congress elections")

      }

      # output
      return(agg_data)

    }

    # Aggregation level: electoral section
    if (level == "sec") {

      # If cod_INE_prov is not include, id is created
      if (!("sec" %in% names(election_data))) {

        election_data <-
          election_data |>
          mutate(cod_INE_ccaa =
                   extract_code(id_INE_poll_station, level = "ccaa"),
                 cod_INE_prov =
                   extract_code(id_INE_poll_station, level = "prov"),
                 id_INE_mun =
                   extract_code(id_INE_poll_station, level = "mun",
                                full_cod = TRUE),
                 cod_mun_district =
                   extract_code(id_INE_poll_station, level = "mun-district"),
                 cod_sec =
                   extract_code(id_INE_poll_station, level = "sec"))

      }


      # CERA summaries
      data_cera <-
        election_data |>
        filter(str_detect(id_INE_mun, "999")) |>
        distinct(id_INE_poll_station, .keep_all = TRUE) |>
        group_by(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                 cod_INE_prov, prov, id_INE_mun, mun) |>
        summarise(census_cera = sum(census_counting),
                  total_ballots_cera = sum(total_ballots),
                  turnout_cera =
                    round(100 * total_ballots_cera / census_cera,
                          prec_round)) |>
        ungroup()

      # Resident population (without CERA)
      pop_res <-
        election_data |>
        filter(!str_detect(id_INE_mun, "999")) |>
        distinct(id_elec, type_elec, date_elec,
                 id_INE_mun, mun, .keep_all = TRUE) |>
        group_by(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                 cod_INE_prov, prov, id_INE_mun, mun) |>
        summarise(pop_res_mun = sum(pop_res_mun)) |>
        ungroup()

      # Aggregation of data
      agg_data <-
        election_data |>
        # Ignore multiple rows due to candidacies
        distinct(id_elec, type_elec, date_elec, id_INE_poll_station,
                 .keep_all = TRUE) |>
        reframe(n_poll_stations = n(),
                  across(c("census_counting", "ballots_1",
                           "ballots_2", "blank_ballots":"total_ballots"), sum),
                  turnout =
                    round(100 * total_ballots / census_counting, prec_round),
                  porc_valid =
                    round(100 * valid_ballots / total_ballots, prec_round),
                  porc_invalid =
                    round(100 * invalid_ballots / total_ballots, prec_round),
                  porc_parties =
                    round(100 * party_ballots / valid_ballots, prec_round),
                  porc_blank =
                    round(100 * blank_ballots / valid_ballots, prec_round),
                  .by = c(id_elec, type_elec, date_elec, cod_INE_ccaa, ccaa,
                          cod_INE_prov, prov, id_INE_mun, mun, cod_mun_district,
                          cod_sec)) |>
        # Join CERA data
        left_join(data_cera,
                  by = c("id_elec", "type_elec", "date_elec", "cod_INE_ccaa",
                         "ccaa", "cod_INE_prov", "prov", "id_INE_mun",
                         "mun")) |>
        # turnout_1 and turnout_2 for municipalities CERA will be NA
        mutate(turnout_1 =
                 round(100 * ballots_1 / (census_counting - census_cera),
                       prec_round),
               turnout_1 = ifelse(is.nan(turnout_1), NA, turnout_1),
               turnout_2 =
                 round(100 * ballots_2 / (census_counting - census_cera),
                       prec_round),
               turnout_2 = ifelse(is.nan(turnout_2), NA, turnout_2)) |>
        # Join pop res data
        left_join(pop_res,
                  by = c("id_elec", "type_elec", "date_elec", "cod_INE_ccaa",
                         "ccaa", "cod_INE_prov", "prov", "id_INE_mun",
                         "mun")) |>
        # Relocate
        relocate(pop_res_mun, .after = mun) |>
        relocate(turnout_1, .after = ballots_1) |>
        relocate(turnout_2, .after = ballots_2) |>
        # Remove CERA rows
        mutate(n_poll_stations = ifelse(mun == "CERA", NA, n_poll_stations))

      # output
      return(agg_data)

      if (by_parties == TRUE) {

        warning("A summary at census section level cannot be done with party ballots for congress elections")

      }

    }

    # Aggregation level: poll station
    if (level == "poll-station") {

      agg_data <- election_data

      # output
      return(agg_data)

    }

}

