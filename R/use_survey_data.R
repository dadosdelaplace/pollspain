#' Calculate Average Polling Errors
#'
#' @description
#' This function calculates the average polling error for each polling firm and party,
#' based on the provided survey, election, and polling data. It allows for optional filtering
#' by specific polling firms and/or parties.
#'
#' @param survey_data A data frame containing survey data with columns:
#'   - `date_elec`: The date of the election.
#'   - `polling_firm`: The name of the polling firm.
#'   - `party`: The political party for which the vote share is estimated.
#'   - `vote_share`: The estimated vote share from the survey.
#'
#' @param election_data A data frame containing election data with columns:
#'   - `date_elec`: The date of the election.
#'   - `abbrev_candidacies`: The abbreviation of the candidacies (parties).
#'   - `total_ballots`: The total number of ballots cast for each party.
#'
#' @param poll_data A data frame containing polling data with columns:
#'   - `date_elec`: The date of the election.
#'   - `total_ballots`: The total number of ballots cast in each polling station.
#'
#' @param filter_polling_firm Optional. A vector of polling firms to filter by. If NULL (default),
#'   no filtering by polling firm is applied.
#'
#' @param filter_party Optional. A vector of parties to filter by. If NULL (default),
#'   no filtering by party is applied.
#'
#' @return A data frame with the average polling error for each polling firm and party,
#'   optionally filtered by the specified polling firms and/or parties. The data frame includes:
#'   - `date_elec`: The date of the election.
#'   - `polling_firm`: The name of the polling firm.
#'   - `party`: The political party.
#'   - `avg_polling_error`: The average polling error for the given firm and party.
#'
#' @examples
#' \dontrun{
#' # Example usage with all data
#' polling_errors <- calculate_polling_errors(survey_data, election_data, poll_data)
#' print(polling_errors)
#'
#' # Example usage with filtering by polling firm
#' polling_errors_firm <- calculate_polling_errors(
#'   survey_data,
#'   election_data,
#'   poll_data,
#'   filter_polling_firm = c("Firm A", "Firm B")
#' )
#' print(polling_errors_firm)
#'
#' # Example usage with filtering by party
#' polling_errors_party <- calculate_polling_errors(
#'   survey_data,
#'   election_data,
#'   poll_data,
#'   filter_party = c("Party X", "Party Y")
#' )
#' print(polling_errors_party)
#'
#' # Example usage with filtering by both polling firm and party
#' polling_errors_filtered <- calculate_polling_errors(
#'   survey_data,
#'   election_data,
#'   poll_data,
#'   filter_polling_firm = c("Firm A"),
#'   filter_party = c("Party X")
#' )
#' print(polling_errors_filtered)
#'}
#' @authors
#' Mikaela De Smedt
#'
#' @export
calculate_polling_errors <- function(survey_data, election_data, poll_data,
                                     filter_polling_firm = NULL, filter_party = NULL) {

  # Total number of votes in the election
  total_ballots_poll_data <- sum(poll_data$total_ballots, na.rm = TRUE)

  # Summarize total_ballots by abbrev_candidacies and date_elec
  election_data_summary <- election_data %>%
    group_by(type_elec, date_elec, abbrev_candidacies) %>%
    summarize(total_party_ballots = sum(total_ballots, na.rm = TRUE), .groups = 'drop')

  # Join the summarized total ballots from election_data with survey data
  survey_data_with_ballots <- survey_data %>%
    inner_join(election_data_summary, by = c("date_elec", "party" = "abbrev_candidacies"))

  # Calculate actual vote share from total ballots
  survey_data_with_ballots <- survey_data_with_ballots %>%
    mutate(actual_vote_share = (total_party_ballots / total_ballots_poll_data) * 100)

  # Calculate polling error
  survey_data_with_ballots <- survey_data_with_ballots %>%
    mutate(polling_error = vote_share - actual_vote_share)

  # Filter by polling firm if specified
  if (!is.null(filter_polling_firm)) {
    survey_data_with_ballots <- survey_data_with_ballots %>%
      filter(polling_firm %in% filter_polling_firm)
  }

  # Filter by party if specified
  if (!is.null(filter_party)) {
    survey_data_with_ballots <- survey_data_with_ballots %>%
      filter(party %in% filter_party)
  }

  # Compute average polling error per polling house and party
  polling_errors_per_party <- survey_data_with_ballots %>%
    group_by(date_elec, polling_firm, party) %>%
    summarize(avg_polling_error = mean(polling_error, na.rm = TRUE), .groups = 'drop')

  return(polling_errors_per_party)
}
