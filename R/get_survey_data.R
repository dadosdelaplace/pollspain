#' @title Summaries of the poll data for a given election.
#'
#' @description Import, preprocess and summary survey data.
#'
#' @inheritParams import_survey_data
#' @param short_version Flag to indicate whether it should be returned
#' a short version of the data (just key variables) or not.
#' Defaults to \code{TRUE}.
#' @param filter_polling_firm,filter_media,filter_abbrev_candidacies
#' Do you want to filter surveys by polling firm, media or abbrev of
#' candidacies? A string vector should be introduced. Defaults to
#' \code{NULL}.
#' @param filter_n_field_days,filter_days_until_elec Do you want to
#' filter surveys by number of fieldwork days or days until election?
#' A single numeric value should be introduced. Defaults to \code{NULL}.
#' @param lower_sample_size,upper_sample_size Do you want to
#' filter surveys by sample size? Single numeric values should be
#' introduced for each one. Defaults to \code{NULL} in both cases.
#' @param lower_fieldwork_date,upper_fieldwork_date Do you want to
#' filter surveys by fieldwork_date? Single date values should be
#' introduced for each one. Defaults to \code{NULL} in both cases.
#'
#' @return A tibble with rows corresponding to the estimated results
#' for each party for each election given by a particular pollster,
#' including the following variables:
#'
#'   \item{id_survey}{survey's id constructed from the polling
#'   firm and the dates for the start and end of the fieldwork.}
#'   \item{id_elec}{election's id constructed from the election code
#'   and date.}
#'   \item{polling_firm}{organisation conducting the poll.}
#'   \item{media}{commissioning organisation / media outlet. Variable
#'    not available for short version.}
#'   \item{n_polls_by_elec}{Number of polls done by each pollster for a
#'   particular election. Variable not available for short version.}
#'   \item{fieldwork_start, fieldwork_end, n_field_days}{fieldwork period
#'   and its length (days).}
#'   \item{days_until_election}{number of days until the next election
#'   counting form the start of the fieldwork. Variable not available
#'   for short version.}
#'   \item{sample_size}{sample size of the survey.}
#'   \item{abbrev_candidacies, name_candidacies_nat}{acronym and full name
#'    of the candidacies at national level. The last one is not available
#'    available for short version.}
#'   \item{id_candidacies_nat}{id for candidacies at national level.}
#'   \item{estimated_porc_ballots}{estimated percentage of ballots
#'   for each party.}
#'
#' @details This function uses the helper function \code{import_survey_data()},
#' which imports and cleans survey data, and adds other relevant variables for
#' analysis. This function gives a tidy table of the survey data that
#' is ready for analysis or visualisation.
#'
#' @author Javier Alvarez-Liebana and David Pereiro-Pol.
#' @keywords summary_survey_data
#' @name summary_survey_data
#' @import crayon
#' @examples
#'
#' ## Correct examples
#'
#' # Summary for all 2019-2023 surveys
#' summary_survey_data(year = c(2019, 2023))
#'
#' # Summary for 2019-2023 surveys in a full version, filtering
#' # just 40DB and GAD3 polling firms, the last 30 days before elec,
#' # and with a sample size at least than 500 people
#' summary_survey_data(year = c(2019, 2023), short_version = FALSE,
#'                     filter_polling_firm = c("GAD3", "40DB"),
#'                     filter_days_until_elec = 30,
#'                     lower_sample_size =  500)
#'
#' # Summary for surveys for the forthcoming elections in, filtering
#' # by parties ("PP" and "PSOE") and with a sample size at least
#' # than 500 people
#' summary_survey_data(forthcoming = TRUE,
#'                     filter_abbrev_candidacies = c("PP", "PSOE"),
#'                     lower_sample_size =  500)
#' \dontrun{
#' # ----
#' # Incorrect examples
#' # ----
#'
#' # Wrong examples
#'
#' # Invalid year
#' summary_survey_data(year = 2018, short_version = FALSE)
#'
#' # Invalid short version flag: short_version should be a
#' # logical variable
#' summary_survey_data(type_elec = "congress", year = 2019,
#'                   short_version = "yes")
#' }
#'
#' @export
summary_survey_data <-
  function(type_elec = "congress", year = NULL, date = NULL,
           verbose = TRUE, short_version = TRUE, format = "long",
           forthcoming = TRUE,
           filter_polling_firm = NULL, filter_media = NULL,
           filter_n_field_days = NULL, filter_days_until_elec = NULL,
           lower_sample_size = NULL, upper_sample_size = NULL,
           lower_fieldwork_date = NULL, upper_fieldwork_date = NULL,
           filter_abbrev_candidacies = NULL) {

    # check by_parties
    if (!is.logical(short_version) | is.na(short_version)) {

      stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

    }

    # Check if verbose is correct
    if (!is.logical(verbose) | is.na(verbose)) {

      stop(red("Ups! `verbose` argument should be a TRUE/FALSE logical flag."))

    }

    if (verbose) {

      message(green("Summary survey data"))
      message(yellow("   [x] Checking if parameters are allowed..."))

    }

    # Check format
    if (!(format %in% c("long", "wide"))) {

      stop(red("Ups! `format` argument should be 'long' or 'wide'."))

    }

    survey_data <-
      import_survey_data(type_elec = "congress", year = year,
                         date = date, verbose = verbose, format = "long",
                         forthcoming = forthcoming)

    # Computing some statistics
    if (verbose) {

      message(blue("   [x] Computing some statistics..."))

    }

    # days until election
    survey_data <-
      survey_data |>
      mutate("days_until_elec" =
               ifelse(str_detect(id_elec, "NA"), NA,
                       as.numeric(as_date(str_remove(id_elec, "^\\d{2}-")) -
                                    fieldwork_start)), .after = n_field_days)

    # number of polls
    number_polls <-
      survey_data |>
      distinct(id_survey, .keep_all = TRUE) |>
      count(polling_firm, id_elec) |>
      rename(n_polls_by_elec = n)
    survey_data <-
      survey_data |>
      left_join(number_polls, by = c("polling_firm", "id_elec")) |>
      relocate(n_polls_by_elec, .after = media)

    if (verbose) {

      message(blue("   [x] Filtering surveys..."))

    }

    # filtering: polling_firm
    if (!is.null(filter_polling_firm)) {

      aux <-
        survey_data |>
        filter(str_to_lower(polling_firm) %in% str_to_lower(filter_polling_firm))
      if (nrow(aux) == 0) {

        message(yellow("Be careful! No data was found for the `filter_polling_firm` provided. No filtering was performed"))

      } else {

        survey_data <- aux

      }
    }

    # filtering: media
    if (!is.null(filter_media)) {

      aux <-
        survey_data |>
        filter(str_to_lower(media) %in% str_to_lower(filter_media))

      if (nrow(aux) == 0) {

        message(yellow("Be careful! No data was found for the `filter_media` provided. No filtering was performed"))

      } else {

        survey_data <- aux

      }
    }

    # filtering: n_field_days
    if (!is.null(filter_n_field_days)) {

      aux <-
        survey_data |>
        filter(n_field_days >= filter_n_field_days)

      if (nrow(aux) == 0) {

        message(yellow("Be careful! No data was found for the `filter_n_field_days` provided. No filtering was performed"))

      } else {

        survey_data <- aux

      }
    }

    # filtering: days_until_elec
    if (!is.null(filter_days_until_elec)) {

      aux <-
        survey_data |>
        filter(days_until_elec <= filter_days_until_elec)

      if (nrow(aux) == 0) {

        message(yellow("Be careful! No data was found for the `filter_days_until_elec` provided. No filtering was performed"))

      } else {

        survey_data <- aux

      }
    }

    # filtering: sample size
    if (!is.null(lower_sample_size) | !is.null(upper_sample_size)) {

      lower_sample_size <- ifelse(is.null(lower_sample_size), 0, lower_sample_size)
      upper_sample_size <- ifelse(is.null(upper_sample_size), Inf, upper_sample_size)

      aux <-
        survey_data |>
        filter(between(sample_size, lower_sample_size, upper_sample_size))

      if (nrow(aux) == 0) {

        message(yellow("Be careful! No data was found for the `lower_sample_size` and `upper_sample_size` provided. No filtering was performed"))

      } else {

        survey_data <- aux

      }
    }

    # filtering: fieldwork
    if (!is.null(lower_fieldwork_date) | !is.null(upper_fieldwork_date) ) {

      lower_fieldwork_date <-
        ifelse(is.null(lower_fieldwork_date), as_date("1970-01-01"), lower_fieldwork_date)
      upper_fieldwork_date <-
        ifelse(is.null(upper_fieldwork_date), as_date("2300-01-01"), upper_fieldwork_date)

      aux <-
        survey_data |>
        filter(between(fieldwork_start, lower_fieldwork_date, upper_fieldwork_date))

      if (nrow(aux) == 0) {

        message(yellow("Be careful! No data was found for the `lower_fieldwork_date` and `upper_fieldwork_date` provided. No filtering was performed"))

      } else {

        survey_data <- aux

      }
    }

    # filtering: abbrev
    if (!is.null(filter_abbrev_candidacies)) {

      aux <-
        survey_data |>
        filter(str_to_upper(abbrev_candidacies) %in%
                 str_to_upper(filter_abbrev_candidacies))

      if (nrow(aux) == 0) {

        message(yellow("Be careful! No data was found for the `filter_abbrev_candidacies` provided. No filtering was performed"))

      } else {

        survey_data <- aux

      }
    }

    # short version
    if (short_version){

      if (verbose) {

        message(yellow("! A short version was asked (if you want all variables, run with `short_version = FALSE`)"))

      }

      survey_data <- survey_data |>
        select(id_survey, id_elec, polling_firm, n_field_days, sample_size,
               fieldwork_start, fieldwork_end, abbrev_candidacies,
               estimated_porc_ballots)



    }

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
