#' @title Get Survey Data from GitHub
#'
#' @description
#' This function fetches and processes survey data from the specified GitHub directory. It downloads and combines RDA files containing survey data from a specified range of years and months, applies various filters based on the user’s parameters, and returns a cleaned and processed data frame.
#'
#' @param from The start year for fetching surveys (default is 1982). The beginning of the period for which survey data should be retrieved.
#' @param to The end year for fetching surveys (default is 2023). The end of the period for which survey data should be retrieved.
#' @param min_days_to Minimum number of days to the election for filtering (default is NULL). If specified, only surveys conducted at least this many days before the election are included.
#' @param max_days_to Maximum number of days to the election for filtering (default is NULL). If specified, only surveys conducted no more than this many days before the election are included.
#' @param select_polling_firm String matching in polling_firm column (default is "all"). If specified, only surveys from polling firms matching this string are included.
#' @param select_media String matching in media column (default is "all"). If specified, only surveys from media outlets matching this string are included.
#' @param select_parties Character vector specifying which columns (parties) to keep (default is "all"). If specified, only columns corresponding to the specified parties are retained.
#' @param min_field_days Minimum number of fieldwork days (default is NULL). If specified, only surveys with at least this many days of fieldwork are included.
#' @param max_field_days Maximum number of fieldwork days (default is NULL). If specified, only surveys with no more than this many days of fieldwork are included.
#' @param min_size Minimum sample size (default is NULL). If specified, only surveys with at least this sample size are included.
#' @param include_media Whether to include media information (default is TRUE). If set to FALSE, media information will be excluded from the results.
#' @param include_exit_polls Whether to include exit polls in the data (default is TRUE). If set to FALSE, exit polls will be excluded from the results.
#'
#' @return
#' Data frame of processed survey data. The resulting data frame includes columns such as \code{polling_firm}, \code{media}, \code{sample_size}, \code{turnout}, \code{fieldwork_start}, \code{fieldwork_end}, \code{date_elec}, \code{fieldwork_duration}, \code{is_exit_poll}, \code{party}, and \code{vote_share}.
#'
#' @details
#' The function retrieves and processes survey data files from a specified GitHub directory, applying user-defined filters to the data. The result is a cleaned and combined data frame with relevant survey information, including optional media and exit poll data.
#'
#' @author
#' Mikaela DeSmedt
#'
#' @importFrom dplyr mutate filter select
#' @importFrom httr GET content stop_for_status
#' @importFrom glue glue
#' @importFrom lubridate dmy as_date
#' @importFrom purrr map compact
#'
#' @examples
#' # Correct usage
#' survey_data <- get_survey_data(from = 1982, to = 1986, min_days_to = 3)
#' print(head(survey_data))
#'
#' # Exclude exit polls
#' survey_data <- get_survey_data(from = 1982, to = 1986, include_exit_polls = FALSE)
#' print(head(survey_data))
#'
#' # Fetching Data from 2000 to 2005 with Maximum Days to Election
#' survey_data_2 <- get_survey_data(from = 2000, to = 2005, max_days_to = 100)
#' print(head(survey_data_2))
#'
#' # Fetching Data from 1990 to 1995 without media
#' survey_data_3 <- get_survey_data(from = 1990, to = 1995, include_media = FALSE)
#' print(head(survey_data_3))
#'
#' # Fetching Data from 1982 to 2020 with Both Minimum and Maximum Days to Election
#' survey_data_4 <- get_survey_data(from = 1982, to = 2020, min_days_to = 10, max_days_to = 200)
#' print(head(survey_data_4))
#'
#' # Fetching Data from 1982 to 2020 with Specific Polling Firm
#' survey_data_5 <- get_survey_data(from = 1982, to = 2020, select_polling_firm = "CIS")
#' print(head(survey_data_5))
#'
#' # Fetching Data from 1982 to 2020 with Specific Media
#' survey_data_6 <- get_survey_data(from = 1982, to = 2020, select_media = "El País")
#' print(head(survey_data_6))
#'
#' # Fetching Data from 1982 to 2020 with Minimum Sample Size
#' survey_data_7 <- get_survey_data(from = 1982, to = 2020, min_size = 1000)
#' print(head(survey_data_7))
#'
#' # Fetching Data from 1982 to 2020 with Specific Parties
#' survey_data_8 <- get_survey_data(from = 1982, to = 2020, select_parties = c("psoe", "pp"))
#' print(head(survey_data_8))
#'
#' # Incorrect usage
#' # End Year Before Start Year
#' # This should raise an error or warning
#' # Reason: The 'to' year is earlier than the 'from' year, which is logically incorrect.
#' \dontrun{
#' survey_data_9 <- get_survey_data(from = 1986, to = 1982)
#' }
#'
#' # Non-Numeric min_days_to Value
#' # This should raise an error
#' # Reason: The min_days_to parameter expects a numeric value, but a string is provided.
#' \dontrun{
#' survey_data_10 <- get_survey_data(from = 1982, to = 2023, min_days_to = "three")
#' }
#'
#' # Fetching Data Beyond Available Date Range
#' # This should raise an error or return an empty data frame
#' # Reason: There are no survey data files available for the specified date range.
#' \dontrun{
#' survey_data_11 <- get_survey_data(from = 1900, to = 1910)
#' }
#'
#' # Invalid Date Range with Future Dates
#' # This should raise an error or warning
#' # Reason: The specified date range includes future years for which no survey data files exist.
#' \dontrun{
#' survey_data_12 <- get_survey_data(from = 2025, to = 2030)
#' }
#'
#' @export

get_survey_data <- function(from = 1982, to = 2023,
                            min_days_to = NULL,
                            max_days_to = NULL,
                            select_polling_firm = "all",
                            select_media = "all",
                            select_parties = "all",
                            min_field_days = NULL,
                            max_field_days = NULL,
                            min_size = NULL,
                            include_media = TRUE,
                            include_exit_polls = TRUE) {
  message("Please wait, depending on your internet connection and period requested this may take a while.")

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

  base_url <- "https://github.com/mikadsr/Pollspain-data/blob/main/survey%20data/"

  # Create a sequence of years and months
  years <- seq(from, to)
  months <- sprintf("%02d", 1:12)

  # Generate all possible file names
  file_names <- expand.grid(year = years, month = months) |>
    mutate(file_name = glue("POLL_02{year}{month}.rda"))

  # Generate URLs
  urls <- file_names |>
    mutate(url = glue("{base_url}{file_name}?raw=true"))

  # Filter out the future dates that don't have files yet
  urls <- urls |>
    dplyr::filter()(as.numeric(year) < to | (as.numeric(year) == to & as.numeric(month) <= month(Sys.Date())))

  # Download and read RDA files
  raw_surveys <- suppressWarnings({
    map(urls$url, ~ {
      tryCatch({
        read_rda_from_github(.x)
      }, error = function(e) {
        NULL
      })
    }) |>
      compact()  # Remove NULLs
  })

  # Combine all data frames, automatically filling missing columns with NA
  raw_surveys <- suppressWarnings(bind_rows(raw_surveys))

  # Ensure the date_elec and fieldwork_end columns are present
  if (!"date_elec" %in% names(raw_surveys)) {
    stop("The column 'date_elec' is not present in the data.")
  }
  if (!"fieldwork_end" %in% names(raw_surveys)) {
    stop("The column 'fieldwork_end' is not present in the data.")
  }

  # Convert date_elec and fieldwork_end to Date objects
  raw_surveys <- raw_surveys |>
    mutate(
      date_elec = lubridate::as_date(date_elec),
      fieldwork_end = lubridate::as_date(fieldwork_end),
      fieldwork_start = lubridate::as_date(fieldwork_start),
      fieldwork_days = as.numeric(difftime(fieldwork_end, fieldwork_start, units = "days"))
    )

  # Apply filtering based on polling_firm
  if (select_polling_firm != "all") {
    raw_surveys <- raw_surveys |>
      dplyr::filter(str_detect(polling_firm, select_polling_firm, ignore.case = TRUE))
  }

  # Apply filtering based on media
  if (select_media != "all" && include_media) {
    raw_surveys <- raw_surveys |>
      dplyr::filter(str_detect(media, select_media, ignore.case = TRUE))
  }

  # Apply filtering based on min_field_days and max_field_days
  if (!is.null(min_field_days)) {
    raw_surveys <- raw_surveys |>
      dplyr::filter(fieldwork_days >= min_field_days)
  }

  if (!is.null(max_field_days)) {
    raw_surveys <- raw_surveys |>
      dplyr::filter(fieldwork_days <= max_field_days)
  }

  # Apply filtering based on min_size
  if (!is.null(min_size)) {
    raw_surveys <- raw_surveys |>
      dplyr::filter(sample_size >= min_size)
  }

  # Calculate days to election and filter based on min_days_to and max_days_to
  raw_surveys <- raw_surveys |>
    mutate(days_to_elec = as.numeric(difftime(date_elec, fieldwork_end, units = "days")))

  if (!is.null(min_days_to)) {
    raw_surveys <- raw_surveys |>
      dplyr::filter(days_to_elec >= min_days_to)
  }

  if (!is.null(max_days_to)) {
    raw_surveys <- raw_surveys |>
      dplyr::filter(days_to_elec <= max_days_to)
  }

  # Apply filtering to exclude exit polls if include_exit_polls is FALSE
  if (!include_exit_polls) {
    raw_surveys <- raw_surveys |>
      dplyr::filter(!is_exit_poll)
  }

  # select specific parties if select_parties is not "all"
  if (select_parties != "all") {
    raw_surveys <- raw_surveys |>
      dplyr::filter(party %in% select_parties)
  }

  # Reorder columns to maintain the original dataset order
  raw_surveys <- raw_surveys |>
    dplyr::select(polling_firm, media, sample_size, turnout, fieldwork_start, fieldwork_end,
           date_elec, fieldwork_duration, is_exit_poll, party, vote_share)

  return(raw_surveys)
}

