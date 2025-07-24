#' @title Summaries of the poll data for
#' a given election.
#'
#' @description Import, preprocess and summary survey data.
#'
#' @inheritParams import_survey_data
#' @param short_version Flag to indicate whether it should be returned
#' a short version of the data (just key variables) or not.
#' Defaults to \code{TRUE}.
#'
#' @return A tibble with rows corresponding to the estimated results
#' for each party for each election given by a particular pollster,
#' including the following variables:
#'
#'   \item{id_survey}{survey's id constructed from the polling
#'   firm and the dates for the start and end of the fieldwork.}
#'   \item{id_elec}{election's id constructed from the election code
#'    and date.}
#'   \item{polling_firm}{organisation conducting the poll.}
#'   \item{media}{commissioning organisation / media outlet.}
#'   \item{n_polls}{polls done by each pollster for a particular election}
#'   \item{fieldwork_start, fieldwork_end, n_field_days}{fieldwork period
#'    and its length (days).}
#'   \item{days_until_election}{number of days until the next election
#'   counting form the start of the fieldwork}
#'   \item{sample_size}{sample size of the survey.}
#'   \item{abbrev_candidacies, name_candidacies_nat}{acronym and full name
#' of the candidacies at national level.}
#'   \item{id_candidacies_nat}{id for candidacies at national level.}
#'   \item{estimated_porc_ballots}{estimated percentage of ballots
#'    for each party.}
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
#' # Summary 2023 survey data at prov level in short version
#'
#' summary_survey_data(year = 2023,
#'                         short_version = TRUE)
#'
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
summary_survey_data <- function(type_elec = "congress",
                               year = NULL, date = NULL,
                               verbose = TRUE, short_version = TRUE) {

  # check by_parties
  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  # Check if verbose is correct
  if (!is.logical(verbose) | is.na(verbose)) {

    stop(red("Ups! `verbose` argument should be a TRUE/FALSE logical flag."))

  }

  if (verbose) {

    message(yellow("... Check if parameters are allowed..."))
    Sys.sleep(1/20)

  }

  # Check date
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

  survey_data <- import_survey_data(type_elec = "congress",
                                    year = year,
                                    date = date,
                                    verbose = FALSE)

  survey_data <- survey_data |>
    mutate("days_until_election" = as.numeric(as_date(str_remove(id_elec, "^\\d{2}-")) -
             fieldwork_start), .after = n_field_days)

  number_polls <- survey_data |>
    distinct(id_survey, .keep_all = TRUE) |>
    count(polling_firm, id_elec) |>
    rename("n_polls" = "n")

  survey_data <- survey_data |>
    left_join(number_polls, by = c("polling_firm", "id_elec")) |>
    relocate("n_polls", .after = media)

  if(short_version){

    survey_data <- survey_data |>
      select(id_survey, id_elec, polling_firm, n_field_days,
             abbrev_candidacies, name_candidacies_nat, estimated_porc_ballots)

    return(survey_data)

  } else{

    return(survey_data)

  }

}
