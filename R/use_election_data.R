#' @title Allocate Seats Using the D'Hondt Method for the Spanish Congress
#'
#' @description
#' This function allocates seats to political parties for the Spanish Congress of Deputies 
#' using the D'Hondt method. It allows for aggregation of seat distribution at the provincial, 
#' autonomous community, or national level.
#'
#' @param last_election_ballots A data frame containing the votes for each polling station.
#' @param level The aggregation level for the final seat distribution. Options are `"prov"` (default) 
#'   for provincial level, `"ccaa"` for autonomous community level, and `"national"` for national level.
#'
#' @return
#' A data frame with the final seat distribution, filtered to remove candidacies with zero seats.
#'
#' @details
#' This function performs the following steps:
#' 1. Aggregates votes by province, autonomous community, and party.
#' 2. Determines the number of seats per province based on the election year using the internal dataset `seat_distribution_congress`.
#' 3. Applies the D'Hondt method to allocate seats to parties.
#' 4. Joins the results with party names and autonomous community names from the internal dataset `cod_INE_mun`.
#' 5. Aggregates the results based on the specified level.
#' 6. Filters out candidacies with zero seats before returning the final result.
#'
#'#' @author Mikaela DeSmedt
#'
#' @examples
#' # Example usage:
#' final_seat_distribution <- allocate_seats_dhondt(last_election_ballots, 
#'                                                  level = "ccaa")
#' print(final_seat_distribution)
#'
#' 
#' @export

allocate_seats_dhondt <- function(last_election_ballots, level = "prov") {
  
  # Step 1: Aggregate votes by province, party, and ccaa
  votes_by_province <- last_election_ballots %>%
    group_by(cod_INE_prov, cod_MIR_ccaa, cod_candidacies_prov) %>%
    summarize(total_votes = sum(ballots), .groups = 'drop')
  
  # Step 2: Determine seats per province for the election year
  election_year <- as.character(year(last_election_ballots$date_elec[1]))
  
  # Access static datasets within the package environment
  seats_per_province <- seat_distribution_congress %>%
    filter(year == election_year) %>%
    select(cod_INE_prov, prov, seats)
  
  # Step 3: Join the aggregated votes with seat distribution data
  votes_with_seats <- votes_by_province %>%
    left_join(seats_per_province, by = "cod_INE_prov")
  
  # Step 4: Filter out candidacies that do not meet the 3% threshold
  votes_with_seats <- votes_with_seats %>%
    group_by(cod_INE_prov) %>%
    mutate(total_votes_prov = sum(total_votes)) %>%
    filter(total_votes / total_votes_prov >= 0.03) %>%
    ungroup()
  
  # Define the D'Hondt allocation function
  distribute_seats_dhondt <- function(votes, num_seats) {
    num_parties <- length(votes)
    quotients <- matrix(0, nrow = num_parties, ncol = num_seats)
    
    for (i in 1:num_parties) {
      quotients[i, ] <- votes[i] / 1:num_seats
    }
    
    sorted_quotients <- sort(as.vector(quotients), decreasing = TRUE)
    cutoff <- sorted_quotients[num_seats]
    
    seat_allocation <- integer(num_parties)
    for (i in 1:num_parties) {
      seat_allocation[i] <- sum(quotients[i, ] >= cutoff)
    }
    
    return(seat_allocation)
  }
  
  # Step 5: Apply the D'Hondt function to each province while preserving columns
  seat_distribution_results <- votes_with_seats %>%
    filter(cod_INE_prov != "99") %>%
    group_by(cod_INE_prov, cod_MIR_ccaa, prov) %>%
    summarize(
      seats_allocated = list(distribute_seats_dhondt(total_votes, unique(seats))),
      party_codes = list(cod_candidacies_prov),
      .groups = 'drop'
    )
  
  # Step 6: Expand the list columns to show seats per party
  seat_distribution_expanded <- seat_distribution_results %>%
    unnest(cols = c(seats_allocated, party_codes))
  
  # Step 7: Join back with party names and get the ccaa column from cod_INE_mun
  final_seat_distribution <- seat_distribution_expanded %>%
    left_join(last_election_ballots %>%
                select(cod_candidacies_prov, abbrev_candidacies, name_candidacies) %>%
                distinct(),
              by = c("party_codes" = "cod_candidacies_prov")) %>%
    left_join(cod_INE_mun %>%
                select(cod_INE_prov, ccaa) %>%
                distinct(),
              by = "cod_INE_prov")
  
  # Step 8: Aggregate results based on the selected level
  if (level == "ccaa") {
    final_seat_distribution <- final_seat_distribution %>%
      group_by(cod_MIR_ccaa, abbrev_candidacies, name_candidacies, ccaa) %>%
      summarize(seats = sum(seats_allocated), .groups = 'drop')
  } else if (level == "national") {
    final_seat_distribution <- final_seat_distribution %>%
      group_by(abbrev_candidacies, name_candidacies) %>%
      summarize(seats = sum(seats_allocated), .groups = 'drop')
  } else {
    final_seat_distribution <- final_seat_distribution %>%
      group_by(cod_INE_prov, prov, cod_MIR_ccaa, abbrev_candidacies, name_candidacies, ccaa) %>%
      summarize(seats = sum(seats_allocated), .groups = 'drop')
  }
  
  # Step 9: Filter out candidacies with 0 seats
  final_seat_distribution <- final_seat_distribution %>%
    filter(seats > 0)
  
  # Return the final seat distribution
  return(final_seat_distribution)
}

#' @title Plot Spanish Congress Election Results
#'
#' @description
#' This function generates a map of Spain, visualizing the election results 
#' for the Spanish Congress of Deputies. The map can display results by province 
#' or autonomous community (CCAA) based on the `level` argument. The function 
#' colors each region according to the party with the most votes in that area.
#'
#' @param election_data A data frame containing the election results. It should include 
#' the following columns:
#' \describe{
#'   \item{cod_INE_prov}{Character. The INE code for provinces.}
#'   \item{prov}{Character. The name of the province.}
#'   \item{cod_MIR_ccaa}{Character. The MIR code for autonomous communities.}
#'   \item{abbrev_candidacies}{Character. The abbreviated name of the political party.}
#'   \item{name_candidacies}{Character. The full name of the political party.}
#'   \item{ccaa}{Character. The name of the autonomous community.}
#'   \item{ballots}{Numeric. The number of ballots received by the party.}
#' }
#' @param level A character string indicating the geographic level to plot. 
#' Options are `"prov"` (default) for province-level results, and `"ccaa"` for 
#' autonomous community-level results.
#' @param colors_url A character string specifying the URL from which to 
#' download the color codes for the political parties. Default is 
#' "https://github.com/mikadsr/Pollspain-data/raw/main/get%20auxiliary%20data/party_colors_hex.rda".
#'
#' @details
#' This function performs the following steps:
#' 1. Downloads the color codes for political parties from the specified URL.
#' 2. Processes the election data to match the required format for plotting.
#' 3. Generates a map of Spain with regions colored according to the party with the most votes.
#' 4. Customizes the map based on the specified `level` parameter.
#'
#' @author Mikaela DeSmedt
#'
#' @return
#' A ggplot2 object showing the election results on a map of Spain.
#'
#' @import ggplot2
#' @import dplyr
#' @import mapSpain
#' @importFrom utils download.file
#' @importFrom utils load
#' @importFrom grDevices colors
#'
#' @examples
#' \dontrun{
#' # Example usage with election data
#' plot_election_results(election_data = a2023, level = "prov")
#' plot_election_results(election_data = a2023, level = "ccaa")
#' }
#'
#' 
#' @export
plot_election_results <- function(election_data, level = "prov", colors_url = "https://github.com/mikadsr/Pollspain-data/raw/main/get%20auxiliary%20data/party_colors_hex.rda") {
  
  # Load necessary packages
  library(ggplot2)
  library(dplyr)
  library(mapSpain)
  
  # Load the province or CCAA map depending on the level
  if (level == "prov") {
    title <- "Spanish Congress Election Results by Province"
    map_data <- esp_get_prov()
    merge_by <- "cpro"
    election_by <- "cod_INE_prov"
  } else if (level == "ccaa") {
    title <- "Spanish Congress Election Results by CCAA"
    map_data <- esp_get_ccaa()
    merge_by <- "codauto"
    election_by <- "cod_MIR_ccaa"
  }
  
  # Load the party colors from the provided URL or a local file
  temp <- tempfile()
  download.file(colors_url, temp, quiet = TRUE)
  load(temp)
  
  # Ensure the province or CCAA codes in both dataframes are characters
  election_data[[election_by]] <- as.character(election_data[[election_by]])
  
  # Aggregate ballots to find the winning party by region
  election_data <- election_data %>%
    group_by(!!sym(election_by), abbrev_candidacies) %>%
    summarize(total_ballots = sum(ballots), .groups = 'drop') %>%
    group_by(!!sym(election_by)) %>%
    slice_max(total_ballots, with_ties = FALSE)
  
  # Merge the map with the election results
  merged_data <- merge(map_data, election_data, by.x = merge_by, by.y = election_by, all.x = TRUE)
  
  # Handle duplicates in case of many-to-many relationships
  party_colors_hex_unique <- party_colors_hex %>%
    distinct(abbrev_candidacies, .keep_all = TRUE)
  
  # Join color data with merged_data
  merged_data <- merged_data %>%
    left_join(party_colors_hex_unique, by = "abbrev_candidacies")
  
  # Plotting the results using the colors from the joined data
  ggplot(merged_data) +
    geom_sf(aes(fill = abbrev_candidacies), color = "white") +
    scale_fill_manual(values = setNames(party_colors_hex_unique$party_color, party_colors_hex_unique$abbrev_candidacies)) +
    labs(title = title, fill = "Winning Party") +
    theme_void() +
    theme(legend.position = "bottom")
}
plot_election_results(election_data = a2023, level = "ccaa")


#' @title Plot Parliamentary Seat Distribution
#'
#' @description
#' This function generates a semicircle plot of parliamentary seat distribution for a given election dataset using the `ggparliament` package. It also fetches custom party colors from an external source to enhance the visual representation of the results.
#'
#' @param election_data A data frame containing election results with the following structure:
#' \itemize{
#'   \item \code{cod_INE_prov}: Province code
#'   \item \code{prov}: Province name
#'   \item \code{cod_MIR_ccaa}: Autonomous community code
#'   \item \code{abbrev_candidacies}: Party abbreviations
#'   \item \code{name_candidacies}: Full party names
#'   \item \code{ccaa}: Autonomous community name
#'   \item \code{seats}: Number of seats won by each party
#' }
#' @param colors_url A string URL pointing to the location of the RDA file containing party colors. Default is set to "https://github.com/mikadsr/Pollspain-data/raw/main/get%20auxiliary%20data/party_colors_hex.rda".
#'
#' @details
#' This function performs the following steps:
#' 1. Downloads the party color information from the specified URL.
#' 2. Processes the election data to prepare it for visualization.
#' 3. Generates a semicircle plot showing the distribution of parliamentary seats among parties.
#' 4. Customizes the plot using the downloaded party colors and visualizes the seat allocation effectively.
#'
#'@author Mikaela DeSmedt, Javier Álvarez-Liébana
#'
#' @return
#' A ggplot object representing the parliamentary seat distribution.
#'
#' @import dplyr
#' @import ggparliament
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data(a)
#' # Plot the seat distribution
#' plot_parliament_distribution(election_data = a)
#' }
#'
#'
#'@export
plot_parliament_distribution <- function(election_data, colors_url = "https://github.com/mikadsr/Pollspain-data/raw/main/get%20auxiliary%20data/party_colors_hex.rda") {
  
  # Step 1: Group the data by abbrev_candidacies and sum the seats
  seat_data <- election_data %>%
    group_by(abbrev_candidacies) %>%
    summarise(seats = sum(seats))
  
  # Step 2: Load party colors from the provided URL or a local file
  temp <- tempfile()
  download.file(colors_url, temp, quiet = TRUE)
  load(temp)
  
  # Step 3: Prepare data for ggparliament
  parliament_data <- parliament_data(
    election_data = seat_data,
    parl_rows = 10, # Adjust the number of rows as needed
    party_seats = seat_data$seats,
    type = "semicircle"
  )
  
  # Step 4: Join color data with parliament_data
  parliament_data <- parliament_data %>%
    left_join(party_colors_hex, by = "abbrev_candidacies")
  
  # Step 5: Create the ggparliament plot
  plot <- ggplot(parliament_data, aes(x = x, 
                                      y = y, 
                                      color = abbrev_candidacies, 
                                      fill = abbrev_candidacies)) +
    geom_parliament_seats() +
    scale_fill_manual(values = setNames(parliament_data$party_color, parliament_data$abbrev_candidacies)) +
    scale_color_manual(values = setNames(parliament_data$party_color, parliament_data$abbrev_candidacies)) +
    theme_ggparliament() +
    labs(
      title = "Spanish Congress Election Results",
      subtitle = "Distribution of Seats by Party"
    ) +
    theme_void()+
    theme(legend.position = "bottom",  legend.title = element_blank())  # This line positions the legend at the bottom
  
  
  # Return the plot
  return(plot)
}
