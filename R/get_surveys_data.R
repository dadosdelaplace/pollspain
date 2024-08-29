#' @title Get Survey Data from Pollspain-data
#'
#' @description
#' This function fetches and processes survey data from the Pollspain-data repository It downloads and combines RDA files containing survey data from a specified year or range of years, applies various filters based on the user’s parameters, and returns a cleaned and processed data frame.
#'
#' @param year A single year or a vector of years for which survey data should be retrieved.
#' @param min_days_to Minimum number of days to the election for filtering (default is NULL). If specified, only surveys conducted at least this many days before the election are included.
#' @param max_days_to Maximum number of days to the election for filtering (default is NULL). If specified, only surveys conducted no more than this many days before the election are included.
#' @param select_polling_firm String matching in polling_firm column (default is "all"). If specified, only surveys from polling firms matching this string are included.
#' @param select_media String matching in media column (default is "all"). If specified, only surveys from media outlets matching this string are included.
#' @param select_parties Character vector specifying which parties to keep (default is "all"). If specified, only columns corresponding to the specified parties are retained.
#' @param min_field_days Minimum number of fieldwork days (default is NULL). If specified, only surveys with at least this many days of fieldwork are included.
#' @param max_field_days Maximum number of fieldwork days (default is NULL). If specified, only surveys with no more than this many days of fieldwork are included.
#' @param min_size Minimum sample size (default is NULL). If specified, only surveys with at least this sample size are included.
#' @param include_media Whether to include media information (default is TRUE). If set to FALSE, media information will be excluded from the results.
#' @param include_exit_polls Whether to include exit polls in the data (default is TRUE). If set to FALSE, exit polls will be excluded from the results.
#'
#' @return
#' A data frame of processed survey data. The resulting data frame includes columns such as \code{polling_firm}, \code{media}, \code{sample_size}, \code{turnout}, \code{fieldwork_start}, \code{fieldwork_end}, \code{date_elec}, \code{fieldwork_duration}, \code{is_exit_poll}, \code{party}, and \code{vote_share}.
#'
#' @details
#' The function retrieves and processes survey data files from a specified GitHub directory, applying user-defined filters to the data. The result is a cleaned and combined data frame with relevant survey information, including optional media and exit poll data. The function now accepts a `year` argument that can be a single year or a vector of years, replacing the previous `from` and `to` parameters. It verifies the availability of `congress` elections in the specified years using the \code{dates_elections_spain} dataset.
#'
#' @author
#' Mikaela DeSmedt
#'
#' @importFrom dplyr mutate filter select
#' @importFrom httr GET content stop_for_status
#' @importFrom glue glue
#' @importFrom lubridate dmy as_date
#' @importFrom purrr map compact discard
#'
#' @examples
#' # Fetch survey data for a specific year
#' survey_data <- get_survey_data(year = 1982, min_days_to = 3)
#' print(head(survey_data))
#'
#' # Exclude exit polls for multiple years
#' survey_data <- get_survey_data(year = c(1982, 1986), include_exit_polls = FALSE)
#' print(head(survey_data))
#'
#' # Fetching Data for a range of years with Maximum Days to Election
#' survey_data_2 <- get_survey_data(year = 2000:2005, max_days_to = 100)
#' print(head(survey_data_2))
#'
#' # Fetching Data for a range of years without media
#' survey_data_3 <- get_survey_data(year = 1990:1995, include_media = FALSE)
#' print(head(survey_data_3))
#'
#' # Fetching Data for specific years with Specific Polling Firm
#' survey_data_4 <- get_survey_data(year = c(1982, 1983), select_polling_firm = "CIS")
#' print(head(survey_data_4))
#'
#' # Fetching Data with Minimum Sample Size
#' survey_data_5 <- get_survey_data(year = 1982:2020, min_size = 1000)
#' print(head(survey_data_5))
#'
#' # Fetching Data with Specific Parties
#' survey_data_6 <- get_survey_data(year = 1982:2020, select_parties = c("psoe", "pp"))
#' print(head(survey_data_6))
#'
#' # Incorrect usage
#' # Fetching Data for a year where no congress election exists
#' # This should raise an error or return an empty data frame
#' \dontrun{
#' survey_data_7 <- get_survey_data(year = 1900)
#' }
#'
#' # Invalid year range
#' # This should raise an error or warning
#' # Reason: The specified years include future dates for which no survey data files exist.
#' \dontrun{
#' survey_data_8 <- get_survey_data(year = 2025:2030)
#' }
#'
#' @export
get_survey_data <- function(year,
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

  # Ensure year is a vector and filter for available elections
  year <- as.numeric(year)
  valid_elections <- dates_elections_spain %>%
    filter(cod_elec == "02", year %in% !!year) %>%
    select(year, month)

  if (nrow(valid_elections) == 0) {
    stop("No 'congress' elections available for the specified year(s).")
  }

  # Generate URLs for available elections
  urls <- valid_elections %>%
    mutate(file_name = glue("POLL_02{year}{sprintf('%02d', month)}.rda")) %>%
    mutate(url = glue("{base_url}{file_name}?raw=true"))

  # Download and read RDA files
  raw_surveys <- suppressWarnings({
    map(urls$url, ~ {
      tryCatch({
        data <- read_rda_from_github(.x)
        if (!"date_elec" %in% names(data)) {
          message(glue("Warning: 'date_elec' is missing in {basename(.x)}"))
          data$date_elec <- NA
        }
        data
      }, error = function(e) {
        message(glue("Failed to load data from {basename(.x)}: {e$message}"))
        NULL
      })
    }) %>%
      discard(is.null)  # Remove NULLs from the list
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
  raw_surveys <- raw_surveys %>%
    mutate(
      date_elec = as.Date(date_elec),
      fieldwork_end = as.Date(fieldwork_end),
      fieldwork_start = as.Date(fieldwork_start),
      fieldwork_days = as.numeric(difftime(fieldwork_end, fieldwork_start, units = "days"))
    )

  # Apply filtering based on polling_firm
  if (select_polling_firm != "all") {
    raw_surveys <- raw_surveys %>%
      filter(str_detect(polling_firm, regex(select_polling_firm, ignore_case = TRUE)))
  }

  # Apply filtering based on media
  if (select_media != "all" && include_media) {
    raw_surveys <- raw_surveys %>%
      filter(str_detect(media, regex(select_media, ignore_case = TRUE)))
  }

  # Apply filtering based on min_field_days and max_field_days
  if (!is.null(min_field_days)) {
    raw_surveys <- raw_surveys %>%
      filter(fieldwork_days >= min_field_days)
  }

  if (!is.null(max_field_days)) {
    raw_surveys <- raw_surveys %>%
      filter(fieldwork_days <= max_field_days)
  }

  # Apply filtering based on min_size
  if (!is.null(min_size)) {
    raw_surveys <- raw_surveys %>%
      filter(sample_size >= min_size)
  }

  # Calculate days to election and filter based on min_days_to and max_days_to
  raw_surveys <- raw_surveys %>%
    mutate(days_to_elec = as.numeric(difftime(date_elec, fieldwork_end, units = "days")))

  if (!is.null(min_days_to)) {
    raw_surveys <- raw_surveys %>%
      filter(days_to_elec >= min_days_to)
  }

  if (!is.null(max_days_to)) {
    raw_surveys <- raw_surveys %>%
      filter(days_to_elec <= max_days_to)
  }

  # Apply filtering to exclude exit polls if include_exit_polls is FALSE
  if (!include_exit_polls) {
    raw_surveys <- raw_surveys %>%
      filter(!is_exit_poll)
  }

  # Select specific parties if select_parties is not "all"
  if (select_parties != "all") {
    raw_surveys <- raw_surveys %>%
      filter(party %in% select_parties)
  }

  # Reorder columns to maintain the original dataset order
  raw_surveys <- raw_surveys %>%
    select(polling_firm, media, sample_size, turnout, fieldwork_start, fieldwork_end,
           date_elec, fieldwork_duration, is_exit_poll, party, vote_share) %>%
    mutate(party = case_when(
      party == "psoe" ~ "PSOE",
      party == "pp" ~ "PP",
      party == "iu" ~ "IU",
      party == "ci_u" ~ "CIU",
      party == "basque_nationalist_party" ~ "BNG",
      party == "erc" ~ "ERC",
      party == "cc" & !is.na(party) ~ "CC-NC",
      party == "hb" & !is.na(party) ~ "HB",
      party == "ea" ~ "EAJ-PNV",
      party == "galician_nationalist_bloc" ~ "BNG",
      party == "uv" ~ "UV",
      party == "pa" ~ "PA",
      party == "cds" ~ "CDS",
      party == "par" ~ "PAR",
      party == "u_py_d" ~ "UPD",
      party == "unidas_podemos" ~ "UP",
      party == "cs" ~ "CS",
      party == "erc_sobiranistes" ~ "ERC",
      party == "pacma" ~ "PACMA",
      party == "eh_bildu" ~ "EH BILDU",
      party == "c_ca" ~ "CC-CN",
      party == "vox" ~ "VOX",
      party == "compromis" ~ "COMPROMIS",
      party == "jx_cat" ~ "JXCAT-JUNTS",
      party == "p_de_cat" ~ "PDECAT-E-CIU",
      party == "ap_pdp_pl" ~ "AP",
      party == "prd" ~ "PRD",
      party == "muc" ~ "MUC",
      party == "ee" ~ "EH BILDU",
      party == "galician_coalition" ~ "CG",
      party == "pce" ~ "PCE",
      party == "andalusian_progress_party" ~ "PAP",
      party == "euskadiko_ezkerra" ~ "PSOE",
      party == "erc_cat_si" ~ "ERC",
      party == "di_l" ~ "DI L",
      party == "cdc" ~ "CDC",
      party == "podemos" ~ "UP",
      party == "iu_u_pe_c" ~ "IU",
      party == "sumar" ~ "SUMAR",
      party == "junts" ~ "JXCAT-JUNTS",
      party == "cup" ~ "CUP",
      party == "bng" ~ "BNG",
      party == "upn" ~ "UPN",
      party == "ev" ~ "EV",
      party == "ucd" ~ "UCD",
      party == "ap" ~ "AP",
      party == "psa_pa" ~ "PA",
      party == "fn" ~ "FN",
      party == "pad" ~ "PAD",
      party == "confederation_of_the_greens" ~ "VERDES",
      party == "ruiz_mateos_group" ~ "ARM",
      party == "iu_lv" ~ "IU",
      party == "galician_nationalist_bloc" ~ "AMAIUR",
      party == "cc_n_ca" ~ "CC-CN",
      party == "regionalist_party_of_cantabria" ~ "PRC",
      party == "mas_pais" ~ "MP",
      party == "cup" ~ "CUP",
      party == "iu_u_pe_c" ~ "IU",
      party == "upyd" ~ "UPD",
      party == "eh_bildu" ~ "EH BILDU",
      party == "cdc" ~ "CDC",
      party == "compromis" ~ "COMPROMIS",
      TRUE ~ as.character(party)  # Retains the original party name if not matched
    ))

  return(raw_surveys)
}
