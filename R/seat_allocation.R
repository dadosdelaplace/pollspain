
#' @title Function to calculate the allocated seats according to the D'Hont method in a given
#' electoral district.
#'
#' @description This function allocates seats to political parties in a given electoral district
#' using the D'Hondt method, a highest averages method for proportional representation.
#' Each party's vote total is divided by a series of divisors (1, 2, 3, ..., up to the number of seats),
#' generating a list of quotients. Seats are assigned one by one to the highest quotients until all
#' seats have been distributed. Only parties that surpass a specified vote threshold
#' (expressed as a proportion of total votes, including blank votes) are eligible for seat allocation.
#
#' @param parties A vector containing the names or code of the parties
#' that participated in the election.
#' @param votes A vector containing the votes received by each party.
#' @param blank_votes A vector or number indicating the number of blank votes.
#' @param nseats A number indicating the number of seats that are going to distributed.
#' @param threshold A number indicating the minimal percentage of votes
#' needed to obtain representation.
#' @param short_version Flag to indicate whether it should be returned
#' a short version of the data (just key variables) or not.
#' Defaults to \code{TRUE}.
#'
#' @returns A tibble or a list of tibbles  with rows corresponding to each party including the following
#' variables:
#' \item{party}{acronym of the candidacies}
#' \item{seats}{number of seats distributed to each party}
#' \item{quotient}{quotients that did not receive a seat}
#'
#' @details The purpose of this helper function is to be used in a general
#' function, \code{seats_allocation()}, to calculate the seats distribution of every
#' electoral district of a given election according to the D'Hont method.
#'
#' @author Javier Alvarez-Liebana, Irene Bosque-Gala and David Pereiro-Pol
#' @keywords seat_allocation
#' @name dhondt_seats
#' @import crayon
#'
#' @examples
#'
#' ## Correct examples
#'
#' ## Seats distribution with D'Hont Method for given vectors of parties and votes
#' ## without the remainder quotients
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100, 200)
#'
#' seats <- dhont_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' \dontrun
#'
#' # Incorrect examples
#'
#' # Different length of parties and votes
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100)
#'
#' seats <- dhont_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' @export
dhondt_seats <- function(parties, votes, blank_votes, nseats, threshold, short_version = TRUE) {

  if(length(parties) != length(votes)){
    stop(red("Ups! `parties` and `votes` should have the same length."))
  }

  if(any(!is.numeric(votes) | is.na(votes))){
    stop(red("Ups! `votes` should be a numeric vector."))
  }

  if(any(!is.numeric(blank_votes))){
    stop(red("Ups! `blank_votes` should be a numeric vector."))
  }

  if(any(!is.numeric(nseats) | is.na(nseats))){
    stop(red("Ups! `nseats` should be a number"))
  }

  if(any(!is.numeric(threshold)) | !between(threshold, 0, 1) | is.na(threshold)){
    stop(red("Ups! `threshold` should be a number between 0 and 1"))
  }

  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  total_votes <- sum(votes) + first(blank_votes)

  threshold_votes <- threshold * total_votes

  data <- tibble(party = parties, votes = votes) |>
    filter(party != "blank", votes > threshold_votes)

  quotients <- map_dfr(
    1:nseats,
    function (x) {tibble(
      party    = data |> pull(party),
      divisor  = x,
      quotient = data |> pull(votes) / x)
      }
    )

  top_quotients <- quotients |> slice_max(quotient, n = nseats, with_ties = FALSE)


  seats <- top_quotients |>
    count(party, name = "seats") |>
    arrange(desc(seats))


  if (short_version) {
    return(seats)
  } else {
    unused_quotients <- quotients |>
      anti_join(top_quotients, by = c("party", "quotient")) |>
      select(party, quotient) |>
      arrange(desc(quotient))

    return(list(
      seats = seats,
      remainders = unused_quotients
    ))
  }
}

#HAMILTON

#' @title Function to calculate the allocated seats according to the Hamilton method in a given
#' electoral district.
#'
#' @description This function allocates seats to political parties in a given electoral district
#' using the Hamilton method (also known as the method of largest remainders).
#' The method first calculates an electoral quota by dividing the total number
#' of votes (including blank votes) by the number of seats to be filled.
#' Each party's vote count is divided by this quota to determine an initial seat
#' allocation (using the floor of the result). Remaining seats are then assigned
#' to parties with the largest fractional remainders until all seats are distributed.
#' Only parties that surpass a given vote threshold (expressed as a proportion of
#' total votes) are considered for seat allocation.
#'
#' @inheritParams dhont_seats
#'
#' @returns A tibble or a list of tibbles  with rows corresponding to each party including the following
#' variables:
#' \item{party}{acronym of the candidacies}
#' \item{seats}{number of seats distributed to each party}
#' \item{remainder}{remainders of the initial division that were not selected for a seat}
#'
#' @details The purpose of this helper function is to be used in a general
#' function, \code{seats_allocation()}, to calculate the seats distribution of every
#' electoral district of a given election according to the Hamilton method.
#'
#' @author Javier Alvarez-Liebana, Irene Bosque-Gala and David Pereiro-Pol
#' @keywords seat_allocation
#' @name hamilton_seats
#' @import crayon
#'
#' @examples
#'
#' ## Correct examples
#'
#' ## Seats distribution with Hamilton method for given vectors of parties and votes
#' ## without the remainder quotients
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100, 200)
#'
#' seats <- hamilton_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' \dontrun
#'
#' # Incorrect examples
#'
#' # Different length of parties and votes
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100)
#'
#' seats <- hamilton_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' @export
hamilton_seats <- function(parties, votes, blank_votes, nseats, threshold = 0, short_version = TRUE) {

  if(length(parties) != length(votes)){
    stop(red("Ups! `parties` and `votes` should have the same length."))
  }

  if(any(!is.numeric(votes) | is.na(votes))){
    stop(red("Ups! `votes` should be a numeric vector."))
  }

  if(any(!is.numeric(blank_votes))){
    stop(red("Ups! `blank_votes` should be a numeric vector."))
  }

  if(any(!is.numeric(nseats) | is.na(nseats))){
    stop(red("Ups! `nseats` should be a number"))
  }

  if(any(!is.numeric(threshold)) | !between(threshold, 0, 1) | is.na(threshold)){
    stop(red("Ups! `threshold` should be a number between 0 and 1"))
  }

  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  total_votes <- sum(votes) + first(blank_votes)
  threshold_votes <- threshold * total_votes

  data <- tibble(party = parties, votes = votes) |>
    filter(party != "blank", votes > threshold_votes)

  quota <- total_votes / nseats

  data <- data |>
    mutate(
      exact_seats = votes / quota,
      initial_seats = floor(exact_seats),
      remainder = exact_seats - initial_seats
    )

  assigned_seats <- sum(data$initial_seats)
  remaining_seats <- nseats - assigned_seats

  data <- data |>
    arrange(desc(remainder)) |>
    mutate(extra_seat = if_else(row_number() <= remaining_seats, 1, 0)) |>
    mutate(total_seats = initial_seats + extra_seat)

  seats <- data |> select(party, total_seats) |> arrange(desc(total_seats))

  if (short_version) {
    return(seats)
  } else {
    remainders <- data |>
      filter(extra_seat == 0) |>
      select(party, remainder) |>
      arrange(desc(remainder))

    return(list(
      seats = seats,
      remainders = remainders
    ))
  }
}


#WEBSTER

#' @title Function to calculate the allocated seats according to the Webster method in a given
#' electoral district.
#'
#' @description This function allocates seats to political parties in a given electoral district
#' using the Webster method (also known as the Sainte-Laguë method), a highest averages
#' method for proportional representation. Each party's total number of votes is divided
#' by a series of odd-numbered divisors (1, 3, 5, ...), generating a list of quotients.
#' The highest quotients are selected sequentially to assign the available seats.
#' Only parties that receive a number of votes exceeding a specified threshold
#' (expressed as a proportion of the total votes, including blank votes) are eligible
#' for seat allocation.
#'
#' @inheritParams dhont_seats
#'
#' @returns A tibble or a list of tibbles  with rows corresponding to each party including the following
#' variables:
#' \item{party}{acronym of the candidacies}
#' \item{seats}{number of seats distributed to each party}
#' \item{remainder}{remainders of the initial division that were not selected for a seat}
#'
#' @details The purpose of this helper function is to be used in a general
#' function, \code{seats_allocation()}, to calculate the seats distribution of every
#' electoral district of a given election according to the Webster method.
#'
#' @author Javier Alvarez-Liebana, Irene Bosque-Gala and David Pereiro-Pol
#' @keywords seat_allocation
#' @name webster_seats
#' @import crayon
#'
#' @examples
#'
#' ## Correct examples
#'
#' ## Seats distribution with Hamilton method for given vectors of parties and votes
#' ## without the remainder quotients
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100, 200)
#'
#' seats <- webster_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' \dontrun
#'
#' # Incorrect examples
#'
#' # Different length of parties and votes
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100)
#'
#' seats <- webster_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' @export
webster_seats <- function(parties, votes, blank_votes, nseats, threshold, short_version = TRUE) {

  if(length(parties) != length(votes)){
    stop(red("Ups! `parties` and `votes` should have the same length."))
  }

  if(any(!is.numeric(votes) | is.na(votes))){
    stop(red("Ups! `votes` should be a numeric vector."))
  }

  if(any(!is.numeric(blank_votes))){
    stop(red("Ups! `blank_votes` should be a numeric vector."))
  }

  if(any(!is.numeric(nseats) | is.na(nseats))){
    stop(red("Ups! `nseats` should be a number"))
  }

  if(any(!is.numeric(threshold)) | !between(threshold, 0, 1) | is.na(threshold)){
    stop(red("Ups! `threshold` should be a number between 0 and 1"))
  }

  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  total_votes <- sum(votes) + first(blank_votes)
  threshold_votes <- threshold * total_votes

  data <- tibble(party = parties, votes = votes) |>
    filter(party != "blank", votes > threshold_votes)

  quotients <- map_dfr(
    seq(1, 2 * nseats - 1, by = 2),
    function (x) {tibble(
      party    = data |> pull(party),
      divisor  = x,
      quotient = data |> pull(votes) / x)
    }
  )

  top_quotients <- quotients |>
    slice_max(quotient, n = nseats, with_ties = FALSE) |>
    arrange(desc(quotient))

  seats <- top_quotients |>
    count(party, name = "seats") |>
    arrange(desc(seats))

  if (short_version) {
    return(seats)
  } else {
    remainders <- anti_join(quotients, top_quotients, by = c("party", "quotient")) |>
      select(party, quotient) |>
      arrange(desc(quotient))

    return(list(
      seats = seats,
      remainders = remainders
    ))
  }
}

# e2019a |> reframe(webster_seats(abbrev_candidacies, ballots, nseats = 20, threshold = 0.03, short_version=FALSE)) |>  View()

#HILL

#' @title Function to calculate the allocated seats according to the Hill method in a given
#' electoral district.
#'
#' @description This function allocates seats to political parties in a given electoral district
#' using the Hill method (also known as the Equal Proportions method), a highest averages
#' approach for proportional representation. Each party's total number of votes is divided
#' by the square root of the product of the number of seats already allocated plus one and
#' that number itself (i.e., √[n(n+1)]), producing a list of quotients. Seats are assigned
#' one at a time to the party with the highest quotient, repeating until all seats are allocated.
#' Only parties surpassing a specified threshold (as a proportion of total votes including blank votes)
#' are considered for allocation.
#'
#' @inheritParams dhont_seats
#'
#' @returns A tibble or a list of tibbles  with rows corresponding to each party including the following
#' variables:
#' \item{party}{acronym of the candidacies}
#' \item{seats}{number of seats distributed to each party}
#' \item{remainder}{remainders of the initial division that were not selected for a seat}
#'
#' @details The purpose of this helper function is to be used in a general
#' function, \code{seats_allocation()}, to calculate the seats distribution of every
#' electoral district of a given election according to the Hill method.
#'
#' @author Javier Alvarez-Liebana, Irene Bosque-Gala and David Pereiro-Pol
#' @keywords seat_allocation
#' @name hills_seats
#' @import crayon
#'
#' @examples
#'
#' ## Correct examples
#'
#' ## Seats distribution with Hill method for given vectors of parties and votes
#' ## without the remainder quotients
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100, 200)
#'
#' seats <- hills_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' \dontrun
#'
#' # Incorrect examples
#'
#' # Different length of parties and votes
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100)
#'
#' seats <- hills_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' @export
hills_seats <- function(parties, votes, blank_votes, nseats, threshold, short_version = TRUE) {

  if(length(parties) != length(votes)){
    stop(red("Ups! `parties` and `votes` should have the same length."))
  }

  if(any(!is.numeric(votes) | is.na(votes))){
    stop(red("Ups! `votes` should be a numeric vector."))
  }

  if(any(!is.numeric(blank_votes))){
    stop(red("Ups! `blank_votes` should be a numeric vector."))
  }

  if(any(!is.numeric(nseats) | is.na(nseats))){
    stop(red("Ups! `nseats` should be a number"))
  }

  if(any(!is.numeric(threshold)) | !between(threshold, 0, 1) | is.na(threshold)){
    stop(red("Ups! `threshold` should be a number between 0 and 1"))
  }

  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  total_votes <- sum(votes) + first(blank_votes)
  threshold_votes <- threshold * total_votes

  data <- tibble(party = parties, votes = votes) |>
    filter(party != "blank", votes > threshold_votes)

  quotients <- map_dfr(
    1:nseats,
    function (x) {tibble(
      party    = data |> pull(party),
      divisor  = sqrt(x * (x + 1)),
      quotient = data |> pull(votes) / divisor)
    }
  )

  top_quotients <- quotients |>
    slice_max(quotient, n = nseats, with_ties = FALSE) |>
    arrange(desc(quotient))

  seats <- top_quotients |>
    count(party, name = "seats") |>
    arrange(desc(seats))


  if (short_version) {
    return(seats)
  } else {
    remainders <- anti_join(quotients, top_quotients, by = c("party", "quotient")) |>
      select(party, quotient) |>
      arrange(desc(quotient))

    return(list(
      seats = seats,
      remainders = remainders
    ))
  }
}

# e2019a |> reframe(hills_seats(abbrev_candidacies, ballots, nseats = 20, threshold = 0.03, short_version=FALSE)) |>  View()

#DEANS

#' @title Function to calculate the allocated seats according to the Dean method in a given
#' electoral district.
#'
#' @description This function allocates seats to political parties in a given electoral district
#' using the Dean method (also known as the Harmonic Mean method), a highest averages
#' method for proportional representation. The method uses divisors based on the harmonic mean
#' of consecutive integers: for each party, its vote total is divided by
#' \eqn{(2s(s+1))/(2s+1)}, where \eqn{s} is the current number of seats allocated to the party.
#' Seats are assigned one at a time to the party with the highest resulting quotient
#' until all seats are distributed. Only parties that exceed a vote threshold
#' (expressed as a proportion of the total votes, including blank votes) are considered eligible.
#'
#' @inheritParams dhont_seats
#'
#' @returns A tibble or a list of tibbles  with rows corresponding to each party including the following
#' variables:
#' \item{party}{acronym of the candidacies}
#' \item{seats}{number of seats distributed to each party}
#' \item{divisor}{divisor used to calculate each quotient}
#' \item{quotient}{quotient calculated for each party and divisor}
#'
#' @details The purpose of this helper function is to be used in a general
#' function, \code{seats_allocation()}, to calculate the seats distribution of every
#' electoral district of a given election according to the Dean method.
#'
#' @author Javier Alvarez-Liebana, Irene Bosque-Gala and David Pereiro-Pol
#' @keywords seat_allocation
#' @name deans_seats
#' @import crayon
#'
#' @examples
#'
#' ## Correct examples
#'
#' ## Seats distribution with Dean method for given vectors of parties and votes
#' ## without the remainder quotients
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100, 200)
#'
#' seats <- deans_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' \dontrun
#'
#' # Incorrect examples
#'
#' # Different length of parties and votes
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100)
#'
#' seats <- deans_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' @export
deans_seats <- function(parties, votes, blank_votes, nseats, threshold, short_version = TRUE) {

  if(length(parties) != length(votes)){
    stop(red("Ups! `parties` and `votes` should have the same length."))
  }

  if(any(!is.numeric(votes) | is.na(votes))){
    stop(red("Ups! `votes` should be a numeric vector."))
  }

  if(any(!is.numeric(blank_votes))){
    stop(red("Ups! `blank_votes` should be a numeric vector."))
  }

  if(any(!is.numeric(nseats) | is.na(nseats))){
    stop(red("Ups! `nseats` should be a number"))
  }

  if(any(!is.numeric(threshold)) | !between(threshold, 0, 1) | is.na(threshold)){
    stop(red("Ups! `threshold` should be a number between 0 and 1"))
  }

  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  total_votes <- sum(votes) + first(blank_votes)
  threshold_votes <- threshold * total_votes

  data <- tibble(party = parties, votes = votes) |>
    filter(party != "blank", votes > threshold_votes)


  divisor_dean <- function(s)
    if_else(s == 0, 1,
           (2 * s * (s + 1)) / (2 * s + 1))

  divisors <- map_dbl(0:(nseats - 1), divisor_dean)

  quotients <- pmap_dfr(
    list(party = data$party, v = data$votes),
    function(party, v) {
      tibble(
        party    = party,
        divisor  = divisors,
        quotient = v / divisors
      )
    }
  )

  allocations <- quotients %>%
    arrange(desc(quotient), party) %>%
    slice_head(n = nseats)

  seats <- allocations %>%
    count(party, name = "seats") %>%
    arrange(desc(seats), party)


  if (short_version) {
    return(seats)
  } else {
    return(list(
      seats = seats,
      seat_allocations = allocations
    ))
  }
}

#ADAMS

#' @title Function to calculate the allocated seats according to the Adams method in a given
#' electoral district.
#'
#' @description This function allocates seats to political parties in a given electoral district
#' using the Adams method, a highest averages method for proportional representation
#' that favors smaller parties. In this method, each party’s vote total is divided
#' by a sequence of integers starting from 1 (i.e., 1, 2, 3, ...), generating a series
#' of quotients. Seats are allocated one at a time to the highest quotients until all
#' seats are distributed. Only parties that surpass a specified vote threshold
#' (expressed as a proportion of total votes, including blank votes) are eligible
#' for seat allocation.
#'
#' @inheritParams dhont_seats
#'
#' @returns A tibble or a list of tibbles  with rows corresponding to each party including the following
#' variables:
#' \item{party}{acronym of the candidacies}
#' \item{seats}{number of seats distributed to each party}
#' \item{divisor}{divisor used to calculate each quotient}
#' \item{quotient}{quotient calculated for each party and divisor}
#'
#' @details The purpose of this helper function is to be used in a general
#' function, \code{seats_allocation()}, to calculate the seats distribution of every
#' electoral district of a given election according to the Adam method.
#'
#' @author Javier Alvarez-Liebana, Irene Bosque-Gala and David Pereiro-Pol
#' @keywords seat_allocation
#' @name adams_seats
#' @import crayon
#'
#' @examples
#'
#' ## Correct examples
#'
#' ## Seats distribution with Adams method for given vectors of parties and votes
#' ## without the remainder quotients
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100, 200)
#'
#' seats <- adams_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' \dontrun
#'
#' # Incorrect examples
#'
#' # Different length of parties and votes
#'
#' parties <- c("PP", "PSOE", "PODEMOS", "VOX")
#' votes <- c(200, 350, 100)
#'
#' seats <- adams_seats(parties = parties, votes = votes, blank_votes = 50, threshold = 0.03)
#'
#' @export
adams_seats <- function(parties, votes, blank_votes, nseats, threshold, short_version = TRUE) {

  if(length(parties) != length(votes)){
    stop(red("Ups! `parties` and `votes` should have the same length."))
  }

  if(any(!is.numeric(votes) | is.na(votes))){
    stop(red("Ups! `votes` should be a numeric vector."))
  }

  if(any(!is.numeric(blank_votes))){
    stop(red("Ups! `blank_votes` should be a numeric vector."))
  }

  if(any(!is.numeric(nseats) | is.na(nseats))){
    stop(red("Ups! `nseats` should be a number"))
  }

  if(any(!is.numeric(threshold)) | !between(threshold, 0, 1) | is.na(threshold)){
    stop(red("Ups! `threshold` should be a number between 0 and 1"))
  }

  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  total_votes <- sum(votes) + first(blank_votes)
  threshold_votes <- threshold * total_votes

  data <- tibble(party = parties, votes = votes) |>
    filter(party != "blank", votes > threshold_votes)

  initial_seats <- tibble(party = data$party, seats = 1)

  remaining_seats <- nseats - nrow(initial_seats)
  if (remaining_seats < 0) {
    stop("More parties than seats. Adams method cannot allocate at least one seat per party.")
  }

  divisors <- 1:remaining_seats

  quotients <- pmap_dfr(
    list(party = data$party, v = data$votes),
    function(party, v) {
      tibble(
        party    = party,
        divisor  = divisors,
        quotient = v / divisor
      )
    }
  )

  allocations <- quotients |>
    arrange(desc(quotient), party) |>
    slice_head(n = remaining_seats)

  extra_seats <- allocations |>
    count(party, name = "extra")

  seats <- initial_seats |>
    left_join(extra_seats, by = "party") |>
    mutate(extra = replace_na(extra, 0),
           seats = seats + extra) |>
    select(party, seats) |>
    arrange(desc(seats))

  if (short_version) {
    return(seats)
  } else {

    remainders <- anti_join(quotients, allocations,
                            by = c("party", "divisor", "quotient")) %>%
      arrange(desc(quotient))

    return(list(
      seats = seats,
      remainders = remainders
    ))
  }
}


#' @title Function to calculate the allocated seats according to the chosen method in a given
#' vector of electoral districts.
#'
#' @param election_data A database containing general election data
#' already provided (by other functions or by the user). Database
#' should contain \code{col_id_elec}, \code{col_id_electoral_district},
#' \code{col_abbrev_candidacies}, \code{col_votes} and \code{col_blank_votes} columns.
#' @param method A string providing the method of apportionment that is going
#' to be used. The allowed values are the following:
#' 'D'Hondt, 'Hamilton', 'Webster', 'Hill' and 'Dean'.
#'  Defaults to \code{"D'Hondt"}.
#' @param threshold A number indicating the minimal percentage of votes
#' needed to obtain representation.
#' @param nseats A number indicating the number of seats that are going to distributed.
#' @param col_id_elec A string proving the column name for the election id.
#' @param col_id_electoral_district A string providing the column name for the
#' electoral district.
#' @param col_abbrev_candidacies A string providing the column name for
#' the name or abbreviation of the candidacies.
#' @param col_votes A string providing the column name for the
#' number of votes of each candidacy.
#' @param col_blank_votes A string providing the column name for the
#' number of blank votes.
#'
#' @returns A tibble with rows corresponding to each party including the following
#' variables:
#' \item{xxx}{name or id  of the electoral district}
#' \item{party}{acronym of the candidacies}
#' \item{seats}{number of seats distributed to each party}
#'
#' @details This function uses the individual method functions to allocate seats
#' in a given set of electoral districts.
#'
#' @author Javier Alvarez-Liebana, Irene Bosque-Gala and David Pereiro-Pol
#' @keywords seat_allocation
#' @name seat_allocation
#' @import crayon
#'
#' @examples
#'
#' ##Correct examples
#'
#' # Allocation according to the D'Hondt method in 2016 taking the province as
#' the electoral district with a threshold of 3%.
#'
#' election_data <-
#' summary_election_data(type_election = "congress", year = 2016, level = "prov")
#'
#' prov_seats <- seat_allocation(election_data = election_data,
#'                               method = "D'Hondt",
#'                               threshold = 0.03)
#'
#' \dontrun
#'
#' ## Wrong examples
#'
#' # Invalid 'method' argument,"invent_method" is not allowed
#' seat_allocation(lection_data = election_data,
#'                               method = "invent_method",
#'                               threshold = 0.03)
#'
#' @export
seat_allocation <- function(election_data, method = "D'Hondt", threshold, nseats = NULL,
                            col_id_elec= "id_elec",
                            col_id_electoral_district = "id_INE_prov",
                            col_abbrev_candidacies = "abbrev_candidacies",
                            col_votes = "ballots",
                            col_blank_votes = "blank_ballots"){
  if(!is.null(nseats)){

    election_data <- election_data |>
      mutate(nseats = nseats)

  } else {

    id_election <- election_data |>
      pull(.data[[col_id_elec]]) |>
      first()

    nseats_year <- total_seats_spain |>
      filter(.data[[col_id_elec]] == id_election) |>
      select(.data[[col_id_electoral_district]], nseats, prov)

    election_data <- election_data |>
      left_join(nseats_year, by = col_id_electoral_district)

  }

  apportion_fun <- switch(method,
                          "D'Hondt"  = dhondt_seats,
                          "Hamilton" = hamilton_seats,
                          "Webster"  = webster_seats,
                          "Hill"     = hills_seats,
                          "Dean"     = deans_seats,
                          "Adams"    = adams_seats)

    seats_results <- election_data |>
      group_by(.data[[col_id_electoral_district]]) |>
      summarise(apportion_fun(parties = .data[[col_abbrev_candidacies]],
                                             votes = .data[[col_votes]],
                                             blank_votes = .data[[col_blank_votes]],
                                             nseats = first(nseats),
                                             threshold = threshold)) |>
      ungroup()


  return(seats_results)

}



#SEATS BY PROVINCES

seats_1982 <- tibble(
  province = c("Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
               "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
               "Baleares", "Las Palmas", "Santa Cruz de Tenerife",
               "Cantabria", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara",
               "Toledo", "Ávila", "Burgos", "León", "Palencia", "Salamanca",
               "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona",
               "Lleida", "Tarragona", "Ceuta", "Alicante", "Castellón", "Valencia",
               "Badajoz", "Cáceres", "La Coruña", "Lugo", "Orense", "Pontevedra",
               "La Rioja", "Madrid", "Melilla", "Murcia", "Navarra", "Álava",
               "Guipúzcoa", "Vizcaya"),
  seats = c(5, 8, 7, 7, 5, 7, 8, 12, 3, 3, 8, 10, 6, 6, 7, 5, 4, 5, 4, 3, 5, 3, 4,
            6, 3, 4, 3, 3, 5, 4, 33, 5, 4, 5, 1, 9, 5, 15, 7, 5, 9, 5, 5, 8, 4, 32,
            1, 8, 5, 4, 7, 10),
  election = 1982)

seats_1986 <- tibble(
  province = c("Álava", "Albacete", "Alicante", "Almería", "Asturias", "Ávila",
               "Badajoz", "Baleares", "Barcelona", "Burgos", "Cáceres", "Cádiz",
               "Cantabria", "Castellón", "Ciudad Real", "Córdoba", "La Coruña",
               "Cuenca", "Girona", "Granada", "Guadalajara", "Guipúzcoa", "Huelva",
               "Huesca", "Jaén", "León", "Lleida", "Lugo", "Madrid", "Málaga", "Murcia",
               "Navarra", "Orense", "Palencia", "Las Palmas", "Pontevedra", "La Rioja",
               "Salamanca", "Santa Cruz de Tenerife", "Segovia", "Sevilla", "Soria",
               "Tarragona", "Teruel", "Toledo", "Valencia", "Valladolid", "Vizcaya",
               "Zamora", "Zaragoza", "Ceuta", "Melilla" ),
  seats = c(4, 4, 10, 5, 9, 3, 6, 6, 33, 4, 5, 9, 5, 5, 5, 7, 9, 3, 5, 7, 3, 7, 5, 3,
            6, 5, 4, 5, 33, 9, 8, 5, 5, 3, 7, 8, 4, 4, 6, 3, 12, 3, 5, 3, 5, 16, 5,
            10, 4, 8, 1, 1),
  election = 1986)

seats_1989 <- tibble(
  province = c("Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
               "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
               "Baleares", "Las Palmas", "Santa Cruz de Tenerife",
               "Cantabria", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara",
               "Toledo", "Ávila", "Burgos", "León", "Palencia", "Salamanca",
               "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona",
               "Lleida", "Tarragona", "Ceuta", "Alicante", "Castellón", "Valencia",
               "Badajoz", "Cáceres", "La Coruña", "Lugo", "Orense", "Pontevedra",
               "La Rioja", "Madrid", "Melilla", "Murcia", "Navarra", "Álava",
               "Guipúzcoa", "Vizcaya"),
  seats = c(5, 9, 7, 7, 5, 6, 10, 12, 3, 3, 7, 9, 6, 7, 7, 5, 4, 5, 3, 3, 5, 3, 4,
            5, 3, 4, 3, 3, 5, 3, 32, 5, 4, 5, 1, 10, 5, 16, 6, 5, 9, 5, 5, 8, 4, 33,
            1, 9, 5, 4, 7, 10),
  election = 1989)

seats_1993 <- tibble(
  province = c("Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
               "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
               "Baleares", "Las Palmas", "Santa Cruz de Tenerife",
               "Cantabria", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara",
               "Toledo", "Ávila", "Burgos", "León", "Palencia", "Salamanca",
               "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona",
               "Lleida", "Tarragona", "Ceuta", "Alicante", "Castellón", "Valencia",
               "Badajoz", "Cáceres", "La Coruña", "Lugo", "Orense", "Pontevedra",
               "La Rioja", "Madrid", "Melilla", "Murcia", "Navarra", "Álava",
               "Guipúzcoa", "Vizcaya"),
  seats = c(5, 9, 7, 7, 5, 6, 10, 12, 3, 3, 7, 9, 7, 7, 7, 5, 4, 5, 3, 3, 5, 3, 4,
            5, 3, 4, 3, 3, 5, 3, 32, 5, 4, 6, 1, 10, 5, 16, 6, 5, 9, 5, 4, 8, 4, 34,
            1, 9, 5, 4, 6, 10),
  election = 1993)

seats_1996 <- tibble(
  province = c("Álava", "Albacete", "Alicante", "Almería", "Asturias", "Ávila",
               "Badajoz", "Baleares", "Barcelona", "Burgos", "Cáceres", "Cádiz",
               "Cantabria", "Castellón", "Ceuta", "Ciudad Real", "Córdoba", "La Coruña",
               "Cuenca", "Girona", "Granada", "Guadalajara", "Guipúzcoa", "Huelva",
               "Huesca", "Jaén", "León", "Lleida", "Lugo", "Madrid", "Málaga", "Melilla", "Murcia", "Navarra", "Orense", "Palencia", "Las Palmas", "Pontevedra", "La Rioja",
               "Salamanca", "Santa Cruz de Tenerife", "Segovia", "Sevilla", "Soria",
               "Tarragona", "Teruel", "Toledo", "Valencia", "Valladolid", "Vizcaya",
               "Zamora", "Zaragoza"),
  seats = c(4, 4, 11, 5, 9, 3, 6, 7, 31, 4, 5, 9, 5, 5, 1, 5, 7, 9, 3, 5, 7, 3, 6, 5, 3,
            6, 5, 4, 4, 34, 10, 1, 9, 5, 4, 3, 7, 8, 4, 4, 7, 3, 13, 3, 6, 3, 5, 16, 5,
            9, 3, 7),
  election = 1996)

seats_2000 <- tibble(
  province = c("Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
               "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
               "Baleares", "Las Palmas", "Santa Cruz de Tenerife",
               "Cantabria", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara",
               "Toledo", "Ávila", "Burgos", "León", "Palencia", "Salamanca",
               "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona",
               "Lleida", "Tarragona", "Ceuta", "Alicante", "Castellón", "Valencia",
               "Badajoz", "Cáceres", "La Coruña", "Lugo", "Orense", "Pontevedra",
               "La Rioja", "Madrid", "Melilla", "Murcia", "Navarra", "Álava",
               "Guipúzcoa", "Vizcaya"),
  seats = c(5, 9, 7, 7, 5, 6, 10, 13, 3, 3, 7, 9, 7, 7, 7, 5, 4, 5, 3, 3, 5, 3, 4,
            5, 3, 4, 3, 3, 5, 3, 31, 5, 4, 6, 1, 11, 5, 16, 6, 5, 9, 4, 4, 8, 4, 34,
            1, 9, 5, 4, 6, 9),
  election = 2000)

seats_2004 <- tibble(
  province = c("Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
               "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
               "Baleares", "Las Palmas", "Santa Cruz de Tenerife",
               "Cantabria", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara",
               "Toledo", "Ávila", "Burgos", "León", "Palencia", "Salamanca",
               "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona",
               "Lleida", "Tarragona", "Ceuta", "Alicante", "Castellón", "Valencia",
               "Badajoz", "Cáceres", "La Coruña", "Lugo", "Orense", "Pontevedra",
               "La Rioja", "Madrid", "Melilla", "Murcia", "Navarra", "Álava",
               "Guipúzcoa", "Vizcaya"),
  seats = c(5, 9, 7, 7, 5, 6, 10, 12, 3, 3, 7, 8, 8, 8, 7, 5, 4, 5, 3, 3, 5, 3, 4,
            5, 3, 4, 3, 3, 5, 3, 31, 6, 4, 6, 1, 11, 5, 16, 6, 4, 9, 4, 4, 7, 4, 35,
            1, 9, 5, 4, 6, 9),
  election = 2004)

seats_2008 <- tibble(
  province = c("Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
               "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
               "Baleares", "Las Palmas", "Santa Cruz de Tenerife",
               "Cantabria", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara",
               "Toledo", "Ávila", "Burgos", "León", "Palencia", "Salamanca",
               "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona",
               "Lleida", "Tarragona", "Ceuta", "Alicante", "Castellón", "Valencia",
               "Badajoz", "Cáceres", "La Coruña", "Lugo", "Orense", "Pontevedra",
               "La Rioja", "Madrid", "Melilla", "Murcia", "Navarra", "Álava",
               "Guipúzcoa", "Vizcaya"),
  seats = c(6, 9, 6, 7, 5, 6, 10, 12, 3, 3, 7, 8, 8, 8, 7, 5, 4, 5, 3, 3, 6, 3, 4,
            5, 3, 4, 3, 2, 5, 3, 31, 6, 4, 6, 1, 12, 5, 16, 6, 4, 8, 4, 4, 7, 4, 35,
            1, 10, 5, 4, 6, 8),
  election = 2008)


seats_2011 <- tibble(
  province = c("Álava", "Albacete", "Alicante", "Almería", "Asturias", "Ávila",
               "Badajoz", "Baleares", "Barcelona", "Burgos", "Cáceres", "Cádiz",
               "Cantabria", "Castellón", "Ciudad Real", "Córdoba", "La Coruña",
               "Cuenca", "Girona", "Granada", "Guadalajara", "Guipúzcoa", "Huelva",
               "Huesca", "Jaén", "León", "Lleida", "Lugo", "Madrid", "Málaga", "Murcia",
               "Navarra", "Orense", "Palencia", "Las Palmas", "Pontevedra", "La Rioja",
               "Salamanca", "Santa Cruz de Tenerife", "Segovia", "Sevilla", "Soria",
               "Tarragona", "Teruel", "Toledo", "Valencia", "Valladolid", "Vizcaya",
               "Zamora", "Zaragoza", "Ceuta",  "Melilla"),
  seats = c(4, 4, 12, 6, 8, 3, 6, 8, 31, 4, 4, 8, 5, 5, 5, 6, 8, 3, 6, 7, 3, 6, 5, 3,
            6, 5, 4, 4, 36, 10, 10, 5, 4, 3, 8, 7, 4, 4, 7, 3, 12, 2, 6, 3, 6, 16, 5,
            8, 3, 7, 1, 1),
  election = 2011)

seats_2015 <- tibble(
  province = c("Álava", "Albacete", "Alicante", "Almería", "Asturias", "Ávila",
               "Badajoz", "Baleares", "Barcelona", "Burgos", "Cáceres", "Cádiz",
               "Cantabria", "Castellón", "Ceuta", "Ciudad Real", "Córdoba", "La Coruña",
               "Cuenca", "Girona", "Granada", "Guadalajara", "Guipúzcoa", "Huelva",
               "Huesca", "Jaén", "León", "Lleida", "Lugo", "Madrid", "Málaga", "Melilla", "Murcia", "Navarra", "Orense", "Palencia", "Las Palmas", "Pontevedra", "La Rioja",
               "Salamanca", "Santa Cruz de Tenerife", "Segovia", "Sevilla", "Soria",
               "Tarragona", "Teruel", "Toledo", "Valencia", "Valladolid", "Vizcaya",
               "Zamora", "Zaragoza"),
  seats = c(4, 4, 12, 6, 8, 3, 6, 8, 31, 4, 4, 9, 5, 5, 1, 5, 6, 8, 3, 6, 7, 3, 6, 5, 3,
            5, 5, 4, 4, 36, 11, 1, 10, 5, 4, 3, 8, 7, 4, 4, 7, 3, 12, 2, 6, 3, 6, 15, 5,
            8, 3, 7),
  election = 2015)

seats_2019a <- tibble(
  province = c("Álava", "Albacete", "Alicante", "Almería", "Asturias", "Ávila",
               "Badajoz", "Baleares", "Barcelona", "Burgos", "Cáceres", "Cádiz",
               "Cantabria", "Castellón", "Ceuta", "Ciudad Real", "Córdoba", "La Coruña",
               "Cuenca", "Girona", "Granada", "Guadalajara", "Guipúzcoa", "Huelva",
               "Huesca", "Jaén", "León", "Lleida", "Lugo", "Madrid", "Málaga", "Melilla", "Murcia", "Navarra", "Orense", "Palencia", "Las Palmas", "Pontevedra", "La Rioja",
               "Salamanca", "Santa Cruz de Tenerife", "Segovia", "Sevilla", "Soria",
               "Tarragona", "Teruel", "Toledo", "Valencia", "Valladolid", "Vizcaya",
               "Zamora", "Zaragoza"),
  seats = c(4, 4, 12, 6, 7, 3, 6, 8, 32, 4, 4, 9, 5, 5, 1, 5, 6, 8, 3, 6, 7, 3, 6, 5, 3,
            5, 4, 4, 4, 37, 11, 1, 10, 5, 4, 3, 8, 7, 4, 4, 7, 3, 12, 2, 6, 3, 6, 15, 5,
            8, 3, 7),
  election = 042019)

seats_2019n <- tibble(
  province = c("Madrid", "Barcelona", "Valencia", "Alicante", "Sevilla", "Málaga",
               "Murcia", "Cádiz", "La Coruña", "Vizcaya", "Islas Baleares", "Las Palmas",
               "Asturias", "Granada", "Pontevedra", "Santa Cruz de Tenerife", "Zaragoza",
               "Almería", "Badajoz", "Córdoba", "Girona", "Guipúzcoa", "Tarragona", "Toledo",
               "Cantabria", "Castellón", "Ciudad Real", "Huelva", "Jaén", "Navarra", "Valladolid",
               "Álava", "Albacete", "Burgos", "Cáceres", "León", "Lleida", "Lugo", "Orense",
               "La Rioja", "Salamanca","Ávila", "Cuenca", "Guadalajara", "Huesca", "Palencia",
               "Segovia", "Teruel", "Zamora","Soria", "Ceuta", "Melilla"),
  seats = c(37, 32, 15, 12, 12, 11, 10, 9, 8, 8, 8, 8, 7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6,
            5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 1),
  election = 112019)

seats_2023 <- tibble(
  province = c("Madrid", "Barcelona", "Valencia", "Alicante", "Sevilla", "Málaga",
               "Murcia", "Cádiz", "La Coruña", "Vizcaya", "Islas Baleares", "Las Palmas",
               "Asturias", "Granada", "Pontevedra", "Santa Cruz de Tenerife", "Zaragoza",
               "Almería", "Córdoba", "Girona", "Guipúzcoa", "Tarragona", "Toledo",
               "Badajoz", "Cantabria", "Castellón", "Ciudad Real", "Huelva", "Jaén",
               "Navarra", "Valladolid","Álava", "Albacete", "Burgos", "Cáceres", "León",
               "Lleida", "Lugo", "Orense", "La Rioja", "Salamanca", "Ávila", "Cuenca",
               "Guadalajara", "Huesca", "Palencia", "Segovia", "Teruel", "Zamora",
               "Soria", "Ceuta", "Melilla"),
  seats = c(37, 32, 16, 12, 12, 11, 10, 9, 8, 8, 8, 8, 7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6,
            5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3,
            2, 1, 1),
  election = 2023
)

total_seats <-  bind_rows(seats_1982, seats_1986, seats_1989, seats_1993, seats_1986,
                          seats_2000, seats_2004, seats_2008,
                          seats_2011, seats_2015, seats_2019a, seats_2019n, seats_2023)

#Apportionment methods by country

apportionment_methods <- data.frame(
  Method = c("D'Hondt", "Hamilton", "Webster", "Hill", "Dean", "Adams"),
  Countries_or_Regions = c(
    "Spain, Portugal, Belgium, Netherlands, Poland, Luxembourg, Austria, Denmark, Russia, Finland, Hungary, Italy (mixed system), Switzerland, Turkey, Northern Ireland",
    "United States (historically)",
    "Germany, Norway, Sweden, Latvia, United States (historically), New Zeland, United Kingdom (some of the counties)",
    "United States (currently)",
    "United States (historically considered)",
    "United States (historically considered)"
  ),
  stringsAsFactors = FALSE
)

apportionment_methods
# https://en.wikipedia.org/wiki/D%27Hondt_method
# NETHERLAND: In the Netherlands, a party must win enough votes for one strictly proportional full seat (note that this is not necessary in plain D'Hondt), which with 150 seats in the lower chamber gives an effective threshold of 0.67%.
