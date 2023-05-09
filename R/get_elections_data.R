
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
#' data("dates_elections_spain")
#' \dontrun{
#' # Right examples
#' mun_census_data <- get_mun_census_data("congress", 2019, 4)
#' mun_census_data <- get_mun_census_data("senate", 2019, 11)
#' mun_census_data <- get_mun_census_data(rep("congress", 3),
#'                                        c(2019, 2019, 2016),
#'                                        c(11, 4, 6))
#' mun_census_data <- get_mun_census_data(c("congress", "senate"),
#'                                        c(2019, 2019), c(11, 4))
#' # Wrong examples
#' mun_census_data <- get_mun_census_data("national", 2019, 4)
#' mun_census_data <- get_mun_census_data("congress", 2016, c(4, 11))
#' mun_census_data <- get_mun_census_data("congress", "2016-06-26")
#' }
#'
#' @export
get_mun_census_data <-
  function(type_elec, year, month) {

    # At this time, just congress election
    if (type_elec != "congress") {

      stop("Development in process: at this time, just congress elections are allowed")

    }

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

    # At this time, just congress election
    if (type_elec != "congress") {

      stop("Development in process: at this time, just congress elections are allowed")

    }

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
    url_raw_data <- "https://raw.githubusercontent.com/dadosdelaplace/pollspain/main/data/csv/pollstation"
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

    # At this time, just congress election
    if (type_elec != "congress") {

      stop("Development in process: at this time, just congress elections are allowed")

    }

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

    # At this time, just congress election
    if (type_elec != "congress") {

      stop("Development in process: at this time, just congress elections are allowed")

    }

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
    url_raw_data <- "https://raw.githubusercontent.com/dadosdelaplace/pollspain/main/data/csv/candidacies_pollstation"
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

    # Recoding parties and incluid id_elec
    candidacies_data <-
      candidacies_data |>
      recod_parties() |>
      mutate(id_elec = glue("{type_to_code_election(type_elec)}-{date_elec}"),
             .before = everything())

    # Remove NA ccaa-prov (CERA summary by ccaa-prov)
    candidacies_data <-
      candidacies_data |>
      drop_na(ccaa, prov)

    # output
    return(candidacies_data)
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
#' @name get_CERA_data
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
get_CERA_data <-
  function(election_data, id_col = "id_INE_poll_station",
           level = "all", cod_CERA = "999", prec_round = 3) {

    # Aggregation of data
    hierarchy_levels <- c("ccaa", "prov", "mun", "mun_district",
                          "sec", "poll_station")
    if (level == "all") {

      levels <- "all"
      group_vars <- "id_elec"

    } else {

      levels <- hierarchy_levels[1:which(hierarchy_levels == level)]
      if (length(levels) <= 3) { # at mun level

        group_vars <- c("id_elec", glue("cod_INE_{levels}"), levels)

      } else {

        group_vars <- c("id_elec", glue("cod_INE_{levels}"), levels[1:3])
      }

    }

    # CERA summaries
    data_cera <-
      election_data |>
      # Just CERA rows
      filter(extract_code(.data[[id_col]], level = "mun") == cod_CERA) |>
      group_by(across(group_vars)) |>
      distinct(.data[[id_col]], .keep_all = TRUE) |>
      reframe(type_elec = unique(type_elec),
              date_elec = unique(date_elec),
              census_cera = sum(census_counting),
              total_ballots_cera = sum(total_ballots),
              turnout_cera =
                round(100 * total_ballots_cera / census_cera, prec_round)) |>
      ungroup()

    # Output
    return(data_cera)

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
#' @name aggregate_election_data
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
aggregate_election_data <-
  function(election_data, level = "all", id_col = "id_INE_poll_station",
           cod_CERA = "999", prec_round = 3) {

    # Remove duplicates
    election_data <-
      election_data |>
      distinct(id_elec, .data[[id_col]], .keep_all = TRUE)

    # extract cod by level
    if (level != "all") {

      hierarchy_levels <- c("ccaa", "prov", "mun", "mun_district",
                            "sec", "poll_station")

      levels <- hierarchy_levels[1:which(hierarchy_levels == level)]

      for (i in 1:length(levels)) {

        election_data <-
          election_data |>
          mutate("cod_INE_{levels[i]}" :=
                   extract_code(.data[[id_col]], level = levels[i]),
                 .after = .data[[id_col]])

      }
    }

    # Aggregation of data
    if (level == "all") {

      levels <- "all"
      group_vars <- "id_elec"

    } else {

      if (length(levels) <= 3) { # at mun level

        group_vars <- c("id_elec", glue("cod_INE_{levels}"), levels)

      } else {

        group_vars <- c("id_elec", glue("cod_INE_{levels}"), levels[1:3])
      }

    }

    # n_poll_stations CERA data
    if (length(levels) <= 2) { # at province level or greater

      n_poll_stations_CERA <-
        election_data |>
        filter(mun == "CERA") |>
        reframe(n_poll_stations = n(), .by = group_vars)

      agg_data <-
        election_data |>
        left_join(n_poll_stations_CERA, by = group_vars) |>
        # Replace NA
        mutate(n_poll_stations = replace_na(n_poll_stations, 0)) |>
        # Summary
        reframe(type_elec = unique(type_elec),
                date_elec = unique(date_elec),
                n_poll_stations =
                  n_distinct(.data[[id_col]]) - unique(n_poll_stations),
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
                .by = group_vars)

    } else {

      agg_data <-
        election_data |>
        # Summary
        reframe(type_elec = unique(type_elec),
                date_elec = unique(date_elec),
                n_poll_stations = n_distinct(.data[[id_col]]),
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
                .by = group_vars) |>
        mutate(n_poll_stations = if_else(mun == "CERA", 0, n_poll_stations))

    }

    agg_data <-
      agg_data |>
      left_join(get_CERA_data(election_data, id_col = id_col,
                              level = level, cod_CERA = cod_CERA,
                              prec_round = prec_round),
                by = group_vars, suffix = c("", ".y")) |>
      select(-contains(".y")) |>
      # turnout_1 and turnout_2 over counting census without cera.
      mutate(turnout_1 =
               round(100 * ballots_1 / (census_counting - census_cera),
                     prec_round),
             turnout_2 =
               round(100 * ballots_2 / (census_counting - census_cera),
                     prec_round), .before = turnout) |>
      # Relocate columns
      relocate(type_elec, date_elec, .after = id_elec)

    # Resident population (without CERA)
    pop_res <-
      election_data |>
      mutate(id_INE_mun =
               extract_code(.data[[id_col]], level = "mun",
                            full_cod = TRUE)) |>
      filter(!str_detect(id_INE_mun, "-999")) |>
      distinct(id_elec, id_INE_mun, .keep_all = TRUE) |>
      summarise(pop_res = sum(pop_res_mun),
                .by = group_vars)

    # Join with pop data
    agg_data <-
      agg_data |>
      # Join pop res data
      left_join(pop_res, by = group_vars) |>
      # Relocate
      relocate(pop_res, .after = date_elec)

    # output
    return(agg_data)

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
#' @name aggregate_candidacies_data
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
aggregate_candidacies_data <-
  function(candidacies_data, level = "all",
           id_col_poll = "id_INE_poll_station",
           id_col_candidacies = "id_candidacies",
           id_col_candidacies_prov = "id_candidacies_prov",
           col_abrev_candidacies = "abbrev_candidacies",
           prec_round = 3) {

    # Remove duplicates
    candidacies_data <-
      candidacies_data |>
      distinct(id_elec, .data[[id_col_poll]], .data[[id_col_candidacies]],
               .keep_all = TRUE)

    # Remove total by ccaa and prov of CERA
    candidacies_data <-
      candidacies_data |>
      drop_na(ccaa, prov)


    # extract cod by level
    if (level != "all") {

      hierarchy_levels <- c("ccaa", "prov", "mun", "mun_district",
                            "sec", "poll_station")

      levels <- hierarchy_levels[1:which(hierarchy_levels == level)]

      for (i in 1:length(levels)) {

        candidacies_data <-
          candidacies_data |>
          mutate("cod_INE_{levels[i]}" :=
                   extract_code(.data[[id_col_poll]], level = levels[i]),
                 .after = .data[[id_col_poll]])

      }
    }

    # Aggregation of data
    if (level == "all") {

      levels <- "all"
      group_vars <- "id_elec"

    } else {

      if (length(levels) <= 3) { # at mun level

        group_vars <- c("id_elec", glue("cod_INE_{levels}"), levels)

      } else {

        group_vars <- c("id_elec", glue("cod_INE_{levels}"), levels[1:3])
      }

    }

    # Use as name the "variant" of party most voted
    group_vars <- c(group_vars, id_col_candidacies)
    most_cand_voted <-
      candidacies_data |>
      reframe(abbrev_candidacies = unique(abbrev_candidacies),
              ballots = sum(ballots),
              .by = c(group_vars, abbrev_candidacies, name_candidacies)) |>
      slice_max(ballots, n = 1, by = group_vars, with_ties = FALSE) |>
      select(-ballots)

    # Candidates elected
    elected <-
      candidacies_data |>
      distinct(across(c(group_vars, prov)), .keep_all = TRUE) |>
      summarise(elected = sum(elected_by_prov),
                .by = group_vars)

    # Aggregate data
    agg_data <-
      candidacies_data |>
      reframe(type_elec = unique(type_elec),
              date_elec = unique(date_elec),
              ballots = sum(ballots),
              .by = group_vars) |>
      # Join with most voted parties: with multiple = "all"
      # we allow, with the same abbrev, different regional names.
      left_join(most_cand_voted, by = group_vars,
                suffix = c(".x", ""), multiple = "all") |>
      select(-contains(".x")) |>
      group_by(across(group_vars)) |>
      distinct(.keep_all = TRUE) |>
      ungroup() |>
      # Join with number of elected
      left_join(elected, by = group_vars) |>
      # Include votes required by elected
      mutate(ballots_by_elec = round(ballots / elected, prec_round),
             ballots_by_elec =
               if_else(is.infinite(ballots_by_elec),
                       NA, ballots_by_elec)) |>
      relocate(type_elec, date_elec, .after = id_elec) |>
      relocate(abbrev_candidacies, name_candidacies,
               .after = id_candidacies)

    # output
    return(agg_data)

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
#' @name get_elections_data
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
get_elections_data <-
  function(type_elec, year, month, level = "all",
           by_parties = TRUE, include_candidacies = FALSE,
           include_candidates = FALSE,
           filter_porc_ballots = NA, filter_elected = NA,
           id_col_poll = "id_INE_poll_station",
           id_col_candidacies = "id_candidacies",
           id_col_candidacies_prov = "id_candidacies_prov",
           col_abrev_candidacies = "abbrev_candidacies",
           prec_round = 3) {

    # Message: yellow for checks
    message(yellow("🔎 Check if parameters are allowed..."))
    Sys.sleep(1/10)

    # At this time, just congress election
    if (type_elec != "congress") {

      stop("Development in process: at this time, just congress elections are allowed")

    }

    # Check: if elections required are allowed
    join_result <-
      dates_elections_spain |>
      inner_join(tibble(cod_elec = type_to_code_election(type_elec),
                        year, month),
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

    # Check: if level takes allowed values
    if (!(level %in% c("all", "ccaa", "prov", "mun",
                       "mun_district", "sec", "poll_station"))) {

      stop("Aggregation level provided by 'level' parameter should be taken from the following values: 'all', 'ccaa', 'prov', 'mun', 'mun_district', 'sec', 'poll_station'")

    }

    # Check: if by_parties is a logical variable
    if (!is.logical(by_parties)) {

      stop(glue("Parameter 'by_parties' must be a logical variable, just TRUE/FALSE are avoided"))

    }

    # Check: if by_parties is a logical variable
    if (by_parties & !include_candidacies) {

      message(red("   🔔 Since include_candidacies = FALSE, aggregating by parties has not been implemented"))
      by_parties <- FALSE

    }

    # Message: blue for get data
    message(blue("📦 Get poll station data..."))
    Sys.sleep(1/10)

    # Message: green details
    message(green("   - Download poll station data..."))
    Sys.sleep(1/10)

    # Getting data at poll station level (without candidacies)
    election_data <-
      get_poll_station_data(type_elec, year, month,
                            prec_round = prec_round)

    # Message: green details
    message(green(glue("   - Aggregating election data at {ifelse(level == 'all', 'national', level)} level...")))
    Sys.sleep(1/10)

    # and then aggregate at provided level
    agg_data <-
      election_data |>
      aggregate_election_data(level = level, id_col = id_col_poll,
                              prec_round = prec_round)

    if (include_candidacies) {

      # Message: blue for get data
      message(blue("📦 Get candidacies (parties) data..."))
      Sys.sleep(1/10)

      # Message: green for details
      message(green("   - Download candidacies (parties) data... (please wait, intensive task)"))
      Sys.sleep(1/10)

      # Getting candidacies data
      candidacies_data <-
        get_candidacies_data(type_elec, year, month,
                             include_candidates = include_candidates)

      # Message: green for details
      message(green(glue("   - Aggregating candidacies data at {ifelse(level == 'all', 'national', level)} level...")))
      Sys.sleep(1/10)

      # and then aggregate at provided level
      agg_data_candidacies <-
        candidacies_data |>
        aggregate_candidacies_data(level = level,
                                   id_col_poll = id_col_poll,
                                   id_col_candidacies = id_col_candidacies,
                                   id_col_candidacies_prov = id_col_candidacies_prov,
                                   col_abrev_candidacies = id_col_candidacies_prov,
                                   prec_round = prec_round)

      # Message: magenta for more
      message(magenta("🖇 Join information..."))
      Sys.sleep(1/10)

      # extract cod by level
      if (level != "all") {

        hierarchy_levels <- c("ccaa", "prov", "mun", "mun_district",
                              "sec", "poll_station")

        levels <- hierarchy_levels[1:which(hierarchy_levels == level)]

      }

      # group vars
      if (level == "all") {

        levels <- "all"
        group_vars <- "id_elec"

      } else {

        if (length(levels) <= 3) { # at mun level

          group_vars <- c("id_elec", glue("cod_INE_{levels}"), levels)

        } else {

          group_vars <- c("id_elec", glue("cod_INE_{levels}"), levels[1:3])
        }

      }


      # Join information
      agg_data <-
        agg_data |>
        left_join(agg_data_candidacies,
                  by = group_vars,
                  suffix = c("", ".y"), multiple = "all") |>
        select(-contains(".y"))

      # Message: yellow-black last message
      message(bgYellow(black("✅ Last summaries and tasks...\n")))
      Sys.sleep(1/10)

      # Including some summaries
      agg_data <-
        agg_data |>
        mutate(porc_candidacies_parties =
                 round(100*ballots/party_ballots, prec_round),
               porc_candidacies_valid =
                 round(100*ballots/valid_ballots, prec_round),
               porc_candidacies_census =
                 round(100*ballots/census_counting, prec_round),
               porc_elected = round(100*elected/360, prec_round),
               anomaly_ballots_elected =
                 round(100*((porc_elected / porc_candidacies_parties) - 1),
                       prec_round))

      if (!is.na(filter_porc_ballots)) {

        agg_data <-
          agg_data |>
          filter(porc_candidacies_valid >= filter_porc_ballots)

      }

      if (!is.na(filter_elected)) {

        agg_data <-
          agg_data |>
          filter(elected >= filter_elected)

      }

      # Relocate
      agg_data <-
        agg_data |>
        select(c(id_elec:pop_res, group_vars[group_vars != "id_elec"],
                 id_candidacies, abbrev_candidacies,
                 name_candidacies, ballots:anomaly_ballots_elected,
                 everything()))
    } else {

      # Message: yellow-black last message
      message(bgYellow(black("✅ Last summaries and tasks...\n")))
      Sys.sleep(1/10)

    }

    # output
    return(agg_data)

  }


