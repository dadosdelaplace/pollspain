#' @title Import survey data
#'
#' @description Import and preprocess surveys data for given election
#' types and dates. This function supports both single values and
#' vector inputs for fetching and combining data for multiple
#' elections at once.
#'
#' @inheritParams import_mun_census_data
#' @param format Do you want the output in \code{format = "long"} or
#' \code{format = "wide"} format? Defaults to \code{"long"}
#' @param forthcoming A flag indicates whether user wants to include
#' surveys for the forthcoming elections. Defaults to \code{TRUE}. If
#' \code{TRUE}, no date neither year are required (in that case, just
#' surveys for the next elections are provided).
#'
#' @return A tibble with rows corresponding to the estimated results
#' for each party for each election given by a particular pollster,
#' including the following variables (long format):
#'
#'   \item{id_survey}{survey's id constructed from the polling
#'   firm and the dates for the start and end of the fieldwork.}
#'   \item{id_elec}{election's id constructed from the election code
#'    and date.}
#'   \item{polling_firm}{organisation conducting the poll.}
#'   \item{media}{commissioning organisation / media outlet.}
#'   \item{fieldwork_start, fieldwork_end, n_field_days}{fieldwork period
#'    and its length (days).}
#'   \item{sample_size}{sample size of the survey.}
#'   \item{abbrev_candidacies, name_candidacies_nat}{acronym and full name
#' of the candidacies.}
#'   \item{id_candidacies_nat}{id for candidacies at national level.}
#'   \item{estimated_porc_ballots}{estimated percentage of ballots
#'    for each party.}
#'
#' @details This function fetches survey data for the specified
#' elections' date by loading the corresponding files from
#' `{pollspaindata}` and processing them by joining the ids and name
#' of the different political parties. Note that dates and years
#' should be associated with years of elections.
#'
#' @author Javier Alvarez-Liebana and David Pereiro Pol
#' @keywords import_survey_data
#' @name import_survey_data
#' @import pollspaindata
#' @examples
#'
#' ## Correct examples
#'
#' # Fetch survey data for congress elections in multiple dates
#' poll_station_data <-
#'   import_survey_data(year = 2023, date = "2019-04-28")
#'
#' # Fetch survey data for congress elections in multiple dates
#' # and for the forthcoming elections
#' poll_station_data <-
#'   import_survey_data(year = c(2011, 2016), date = "2019-04-28",
#'                      forthcoming = TRUE)
#'
#' ## Fetch survey data for congress elections in multiple dates
#' # and for the forthcoming elections, in wide format
#' poll_station_data <-
#'   import_survey_data(year = c(2011, 2016), date = "2019-04-28",
#'                      forthcoming = TRUE, format = "wide")
#'
#' # ----
#' # Incorrect examples
#' # ----
#'
#' \dontrun{
#' # Wrong examples
#'
#' # Invalid election type: "national" is not a valid election type
#' import_survey_data(type_elec = "national", year = 2019)
#'
#' # Invalid election: no congress elections are available in 2018
#' # Please check dataset dates_elections_spain
#' import_survey_data(type_elec = "congress", year = 2018)
#'
#' # Invalid date format: date should be in %Y-%m-%d format
#' import_survey_data(date = "26-06-2016")
#'
#' }
#'
#' @export
import_survey_data <-
  function(type_elec = "congress", year = NULL, date = NULL,
           verbose = TRUE, format = "long", forthcoming = TRUE) {

    # Check if verbose is correct
    if (!is.logical(verbose) | is.na(verbose)) {

      stop(red("Ups! `verbose` argument should be a TRUE/FALSE logical flag."))

    }

    if (verbose) {

      message(green("Import survey data"))
      message(yellow("   [x] Checking if parameters are allowed..."))

    }

    # Check forthcoming
    if (!is.logical(forthcoming) | !(forthcoming %in% c(TRUE, FALSE))) {

      stop(red("Ups! `forthcoming` argument should be a logical TRUE/FALSE variable."))

    }

    # Check format
    if (!(format %in% c("long", "wide"))) {

      stop(red("Ups! `format` argument should be 'long' or 'wide'."))

    }

    # Check date
    if (!is.null(date)) {
      if (!all(str_detect(date, "^\\d{4}-\\d{2}-\\d{2}$")) | any(is.na(date))) {

        stop(red("Ups! If date was provided, `date` should be in format '2000-01-01' (%Y-%m-%d)"))

      } else {

        date <- as_date(date)

      }

    } else if (!is.numeric(year)) {

      if (forthcoming) {

        if (verbose) {

          message(yellow("Be careful! No date neither year was provided: just surveys for the forthcoming elections were provided"))

        }

      } else {

        stop(red("Ups! If no date was provided, `year` should be a numerical variable."))

      }
    }

    allowed_elections <- detect_years(year = year, date = date)

    if (verbose) {

      # Print the file URL for debugging purposes
      message(blue("   [x] Importing survey data ..."))

    }

    if (!is.null(year) | !is.null(date)) {
      if (allowed_elections |> nrow() == 0) {

        stop(red(glue("Ups! No {type_elec} surveys are available. Please, be sure that arguments and dates are right")))

      } else {

        survey_data <-
          pollspaindata::historical_surveys |>
          filter(id_elec %in% glue("{allowed_elections$cod_elec}-{allowed_elections$date}"))

      }
    }

    # forthcoming surveys
    if (forthcoming) {

      if ("survey_data" %in% ls()) {

        survey_data <-
          survey_data |>
          bind_rows(pollspaindata::new_surveys)

      } else {

        survey_data <- pollspaindata::new_surveys

      }
    }

    dict_parties <-
      global_dict_parties |>
      select(id_elec, abbrev_candidacies, id_candidacies_nat, name_candidacies_nat) |>
      arrange(id_elec, abbrev_candidacies) |>
      distinct(id_elec, abbrev_candidacies, .keep_all = TRUE)

    if (forthcoming) {

      dict_parties <-
        dict_parties |>
        bind_rows(dict_parties |>
                    mutate("date" = as_date(str_sub(id_elec, start = 4))) |>
                    slice_max(date, with_ties = FALSE) |>
                    select(-date) |>
                    mutate("id_elec" = max(unique(pollspaindata::new_surveys$id_elec))))
    }

    survey_data <-
      survey_data |>
      left_join(dict_parties,
                by = c("id_elec", "abbrev_candidacies"),
                suffix = c("", ".rm"), copy = TRUE) |>
      select(-contains(".rm")) |>
      relocate(id_candidacies_nat, name_candidacies_nat,
               .after = abbrev_candidacies) |>
      distinct(id_survey, id_candidacies_nat, .keep_all = TRUE)

    if (format == "wide") {

      if (verbose) {

        # Print the file URL for debugging purposes
        message(blue("   [x] Converting to wide data ..."))

      }
      survey_data <-
        survey_data |>
        pivot_wider(names_from = "abbrev_candidacies",
                    values_from = "estimated_porc_ballots",
                    values_fn = sum,
                    values_fill = 0)

    }
    return(survey_data)
}

