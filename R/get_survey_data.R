#' @title Summaries of the poll data for a given election.
#'
#' @description Import, preprocess and summary survey data, for given
#' election types and dates. This function supports both single values
#' and vector inputs for fetching and combining data for multiple
#' elections at once. Surveys for the forthcoming elections can be
#' also asked. Different filtering arguments (polling firm, days
#' until elections, candidacies, sample size, etc) are also included
#' to design a properly query.
#'
#' @inheritParams import_survey_data
#' @param short_version Flag to indicate whether it should be returned
#' a short version of the data (just key variables) or not.
#' Defaults to \code{TRUE}.
#' @param rm_exit_polls Flag to indicate whether exit polls should be
#' removed or not. Defaults to \code{TRUE}.
#' @param rm_unpublish_polls Flag to indicate whether unpublished
#' polls before elections should be removed or not. In Spain, the
#' Electoral Law (LOREG) establishes that it is forbidden to publish,
#' disseminate, or reproduce electoral polls during the five days
#' prior to election day. Defaults to \code{FALSE}.
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
#' @details This function uses the helper function
#' \code{import_survey_data()}, which imports and cleans survey data,
#' and adds other relevant variables for analysis. This function gives
#' a tidy table of the survey data for analysis or visualization. Note
#' that dates and years should be associated with years of elections.
#'
#' @author Javier Alvarez-Liebana and David Pereiro-Pol.
#' @keywords summary_survey_data
#' @name summary_survey_data
#' @import crayon
#' @examples
#'
#' ## Correct examples
#'
#' # Summary of surveys for 2016 and 2023, and 2019-04-28 elections.
#' summary_survey_data(year = c(2016, 2023), date = "2019-04-28")
#'
#' # Summary for all 2019-2023 surveys in a full version, filtering
#' # just 40DB and GAD3 polling firms, the last 30 days before elec,
#' # and with a sample size at least than 500 people
#' summary_survey_data(year = c(2019, 2023), short_version = FALSE,
#'                     filter_polling_firm = c("GAD3", "40DB"),
#'                     filter_days_until_elec = 30,
#'                     lower_sample_size =  500)
#'
#' # Summary for surveys for the forthcoming elections in, filtering
#' # by parties ("PP" and "PSOE") and with a sample size of at least
#' # 500 people
#' summary_survey_data(forthcoming = TRUE,
#'                     filter_abbrev_candidacies = c("PP", "PSOE"),
#'                     lower_sample_size =  500)
#'
#' # Summary for 2023 surveys in a full version, filtering
#' # just 40DB and GAD3 polling firms, surveys that go from the
#' # first of March of 2020 to the first of August of the same year
#' summary_survey_data(year = 2023, short_version = FALSE,
#'                     filter_polling_firm = c("GAD3", "40DB"),
#'                     lower_fieldwork_date = "2020-03-01",
#'                     upper_fieldwork_date = "2020-08-01")
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
#'                    short_version = "yes")
#'
#' # upper_fieldwork_date prior to lower_fieldwork_date
#' summary_survey_data(year = 2023,
#'                     lower_fieldwork_date = "2020-08-01",
#'                     upper_fieldwork_date = "2020-03-01")
#'
#' # upper_sample_size smaller than lower_sample-size
#' summary_survey_data(year = 2023, lower_sample_size = 500,
#'                     upper_sample_size = 200)
#'
#' # Invalid class for argument filter_polling_firm
#' summary_survey_data(year = 2023, filter_polling_firm = 700)
#'
#' # Invalid class for argument filter_media
#' summary_survey_data(year = 2023, filter_media = 700)
#'
#' # Invalid n_fields_days
#' summary_survey_data(year = 2023, filter_n_field_days = -1)
#'
#' }
#' @export
summary_survey_data <-
  function(type_elec = "congress", year = NULL, date = NULL,
           verbose = TRUE, short_version = TRUE, format = "long",
           forthcoming = FALSE,
           rm_exit_polls = TRUE, rm_unpublish_polls = FALSE,
           filter_polling_firm = NULL, filter_media = NULL,
           filter_n_field_days = NULL, filter_days_until_elec = NULL,
           lower_sample_size = NULL, upper_sample_size = NULL,
           lower_fieldwork_date = NULL, upper_fieldwork_date = NULL,
           filter_abbrev_candidacies = NULL) {

    # check if short_version is correct
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

    if (!is.logical(forthcoming) | is.na(forthcoming)) {

      stop(red("Ups! `forthcoming` argument should be a TRUE/FALSE variable."))

    }

    if (!is.logical(rm_exit_polls) | is.na(rm_exit_polls)) {

      stop(red("Ups! `rm_exit_polls` argument should be a TRUE/FALSE variable."))

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

        message(yellow("Be careful! No date neither year was provided: just surveys for the forthcoming elections were provided"))

      } else {

        stop(red("Ups! If no date was provided, `year` should be a numerical variable or `forthcoming` shoulb be set as TRUE."))

      }
    }

    # Check format
    if (!(format %in% c("long", "wide"))) {

      stop(red("Ups! `format` argument should be 'long' or 'wide'."))

    }

    # check filter_polling_firm
    if (!all(is.na(filter_polling_firm)) & !all(is.character(filter_polling_firm))) {

      stop(red("Ups! `filter_polling_firm` argument should be NA or a string of characters."))

    }

    # check filter_media
    if (!all(is.na(filter_media)) & !all(is.character(filter_media))) {

      stop(red("Ups! `filter_media` argument should be NA or a string of characters."))

    }

    # check filter_abbrev_candidacies
    if (!all(is.na(filter_abbrev_candidacies)) & !all(is.character(filter_abbrev_candidacies))) {

      stop(red("Ups! `filter_abbrev_candidacies` argument should be NA or a string of characters."))

    }

    # check filter_n_field_days
    if (!all(is.null(filter_n_field_days)) & !all(is.numeric(filter_n_field_days))) {

      stop(red("Ups! `filter_n_field_days` argument should be a single numeric value"))

    }

    if (!is.null(filter_n_field_days)) {

      if (filter_n_field_days < 0) {

        stop(red("Ups! `filter_n_field_days` argument should be positive"))

      }
    }

    # check filter_days_until_elec
    if (!all(is.null(filter_days_until_elec)) & !all(is.numeric(filter_days_until_elec))) {

      stop(red("Ups! `filter_days_until_elec` argument should be a single numeric value"))

    }

    if (!is.null(filter_days_until_elec)) {

      if (filter_days_until_elec < 0) {

        stop(red("Ups! `filter_days_until_elec` argument should be positive"))

      }
    }

    # check lower and upper sample_size
    if (!all(is.na(c(lower_sample_size, upper_sample_size))) & !all(is.numeric(c(lower_sample_size, upper_sample_size)))) {

      stop(red("Ups! `lower_sample_size` and `upper_sample_size` arguments should be a single numeric value"))

    } else if (!is.null(lower_sample_size) & !is.null(upper_sample_size)) {

      if (lower_sample_size > upper_sample_size) {

        stop(red("Ups! `lower_sample_size` should be smaller or equal than `upper_sample_size`"))

      }

    }

    # check lower and upper fieldwork_date
    if (!all(str_detect(c(lower_fieldwork_date, upper_fieldwork_date), "^\\d{4}-\\d{2}-\\d{2}$")) & !all(is.na(c(lower_sample_size, upper_fieldwork_date)))) {

      stop(red("Ups! `lower_fieldwork_day` and `upper_fieldwork_day` arguments should be should be in format '2000-01-01' (%Y-%m-%d)"))

    } else if (!is.null(lower_fieldwork_date) & !is.null(upper_fieldwork_date)) {

      if (as_date(lower_fieldwork_date) > as_date(upper_fieldwork_date)) {

        stop(red("Ups! `lower_fieldwork_day` should be prior to `upper_fieldwork_day`"))

      }

    }

    survey_data <-
      import_survey_data(type_elec = "congress", year = year,
                         date = date, verbose = FALSE, format = "long",
                         forthcoming = forthcoming)

    # Computing some statistics
    if (verbose) {

      message(blue("   [x] Computing some statistics..."))

    }

    # days until election
    survey_data <-
      survey_data |>
      mutate("days_until_elec" =
               ifelse(str_detect(id_elec, "NA"), NA, str_remove(id_elec, "^\\d{2}-"))) |>
      mutate("days_until_elec" =
               ifelse(is.na(days_until_elec), NA,
                      as.numeric(as_date(days_until_elec) -
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

    filters <- list(
  filter_polling_firm, filter_media, filter_n_field_days, filter_days_until_elec,
  filter_abbrev_candidacies, lower_sample_size, upper_sample_size,
  lower_fieldwork_date, upper_fieldwork_date)

    if (verbose & any(!vapply(filters, is.null, logical(1)))) {

      message(blue("   [x] Filtering surveys..."))

    }

    # removing exits polls and unpublished polls
    if (rm_exit_polls) {

      survey_data <-
        survey_data |>
        filter(days_until_elec > 0)
    }

    if (rm_unpublish_polls) {

      survey_data <-
        survey_data |>
        filter(days_until_elec > 5)
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
        filter(between(fieldwork_start, as_date(lower_fieldwork_date), as_date(upper_fieldwork_date)))

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
               fieldwork_start, fieldwork_end, abbrev_candidacies, id_candidacies_nat,
               name_candidacies_nat, estimated_porc_ballots)

    }

    if (format == "wide") {

      if (verbose) {

        # Print the file URL for debugging purposes
        message(blue("   [x] Converting to wide data ..."))

      }
      survey_data <-
        survey_data |>
        select(-c(id_candidacies_nat, name_candidacies_nat)) |>
        pivot_wider(names_from = "abbrev_candidacies",
                    values_from = "estimated_porc_ballots",
                    values_fn = sum,
                    values_fill = 0)

    }

    return(survey_data)
}
