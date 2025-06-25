
#' @title Function to compute the allocated seats according to the
#' D'Hondt method in a given electoral district.
#'
#' @description This function allocates seats to political parties
#' within a given electoral district using the D'Hondt method, a highest
#' averages method for proportional representation.
#' Each party's total vote (total number of ballots) is divided by a
#' series of divisors (1, 2, 3, ..., to the number of available seats),
#' generating a list of quotients. Seats are then allocated one by one
#' to the highest quotients until all seats have been distributed.
#' Only parties that surpass a specified vote threshold (expressed as
#' a proportion of total votes for a given electoral district,
#' including blank votes) are eligible for seat allocation.
#'
#' According to Spain's Organic Law of the General Electoral Regime
#' (LOREG, Article 163.1), if there is a tie in the quotients between
#' candidacies, the first tie is resolved by drawing lots, and
#' subsequent ties are resolved alternately. In this package, in
#' order to ensure reproducibility, ties will be broken by ordering
#' from highest to lowest number of absolute votes.
#
#' @param candidacies A vector containing one of the following
#' variable: unique codes or abbreviations of the candidacies
#' that participated in the election.
#' @param ballots A vector containing the absolute number of ballots
#' (integer positive values) received by each candidacies
#' @param blank_ballots A numerical value indicating the number of
#' blank ballots (integer positive values).
#' @param n_seats An integer positive value indicating the number of
#' seats that are going to distributed for a given electoral district.
#' @param threshold A numerical value (between 0 and 1) indicating the
#' minimal percentage of votes needed to obtain representation for a
#' given electoral district. Defaults to \code{0.03}.
#' @param short_version Flag to indicate whether it should be returned
#' a short version of the data (just key variables) or not. Defaults
#' to \code{TRUE}.
#'
#' @returns A tibble with rows corresponding to each party including
#' the following variables:
#' \item{candidacies}{abbrev or id of the candidacies}
#' \item{seats}{number of seats}
#' \item{ballots}{absolute number of ballots, just in long format}
#' \item{porc_seats}{percentage of seats respect to the number of seats,
#' just in long format}
#' \item{porc_ballots}{percentage of ballots respect to party ballots
#' (including blank ballots), just in long format}
#' \item{quotient_x}{intermediate quotients of the allocation process}
#'
#' @details The purpose of this helper function is to be used in a general
#' function, \code{seats_allocation()}, to calculate the seats distribution of every
#' electoral district of a given election according to the D'Hont method.
#'
#' @author David Pereiro-Pol, Irene Bosque-Gala and Javier
#' Alvarez-Liebana.
#' @keywords seat_allocation
#' @name dhondt_seats
#' @import crayon
#'
#' @examples
#'
#' # Correct examples
#'
#' # Seats distribution with D'Hondt method for given vectors of
#' # parties and ballots without the remainder quotients
#' # (threshold 0.03 by default) in a short version
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100, 200)
#' seats <- dhondt_seats(candidacies, ballots, blank_ballots = 50,
#'                       n_seats = 5)
#'
#' # Same results in a long version (providing quotients)
#' seats <- dhondt_seats(candidacies, ballots, blank_ballots = 50,
#'                       n_seats = 5, short_version = FALSE)
#'
#' # D'Hondt with threshold 0.05
#' seats <- dhondt_seats(candidacies, ballots, blank_ballots = 50,
#'                       n_seats = 5, threshold = 0.05)
#'
#' # A very high threshold that only one party meets
#' seats <- dhondt_seats(candidacies, ballots, blank_ballots = 50,
#'                       n_seats = 5, threshold = 0.3)
#'
#' \dontrun{
#'
#' # ----
#' # Incorrect examples
#' # ----
#'
#' # Different length of candidacies
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100)
#' seats <- dhondt_seats(candidacies = candidacies, ballots = ballots,
#'                       n_seats = 5, blank_ballots = 50,
#'                       threshold = 0.03)
#'
#' # Ballots with missing values
#' candidacies <- c("PP", "PSOE", "PODEMOS")
#' ballots <- c(200, 350, NA)
#' seats <- dhondt_seats(candidacies = candidacies, ballots = ballots,
#'                       n_seats = 5, blank_ballots = 50,
#'                       threshold = 0.03)
#'
#' # Ballots with char values
#' candidacies <- c("PP", "PSOE", "PODEMOS")
#' ballots <- c("200", "350", "100")
#' seats <- dhondt_seats(candidacies = candidacies, ballots = ballots,
#'                       n_seats = 5, blank_ballots = 50,
#'                       threshold = 0.03)
#'
#' # Threshold should be a numerical value between 0 and 1
#' candidacies <- c("PP", "PSOE", "PODEMOS")
#' ballots <- c(200, 350, 100)
#' seats <- dhondt_seats(candidacies = candidacies, ballots = ballots,
#'                       n_seats = 5, blank_ballots = 50,
#'                       threshold = 3)
#' }
#' @export
dhondt_seats <-
  function(candidacies, ballots, blank_ballots, n_seats,
           threshold = 0.03, short_version = TRUE) {

    if (length(candidacies) != length(ballots)) {

      stop(red("Ups! `candidacies` and `ballots` should have the same length"))

    }

    if (length(unique(candidacies)) != length(candidacies)) {

      stop(red("Ups! `candidacies` should contain unique values"))

    }

    if (any(!is.numeric(ballots) | is.na(ballots))) {

      stop(red("Ups! `ballots` should be a numeric vector without missing"))

    }

    if (any(ballots < 0) | any(floor(ballots) != ballots)) {

      stop(red("Ups! `ballots` should be a numeric vector with positive integer values"))

    }

    if (any(!is.numeric(blank_ballots))) {

      stop(red("Ups! `blank_ballots` should be a numeric varible"))

    }

    if (any(blank_ballots < 0) |
        any(floor(blank_ballots) != blank_ballots)) {

      stop(red("Ups! `blank_ballots` should be a numeric variable with positive integer values"))

    }

    if (length(unique(blank_ballots)) > 1) {

      stop(red("Ups! `blank_ballots` should be just a numeric value or a repeated numerical vector"))

    }

    if (any(!is.numeric(n_seats) | is.na(n_seats) | (length(n_seats) > 1))) {

      stop(red("Ups! `n_seats` should be a single numerical value"))

    }

    if (n_seats < 0 | (floor(n_seats) != n_seats)) {

      stop(red("Ups! `n_seats` should be an integer positive value"))

    }

    if (!is.numeric(threshold) | !between(threshold, 0, 1) |
        is.na(threshold) | (length(threshold) > 1)) {

      stop(red("Ups! `threshold` should be a single number between 0 and 1"))

    }

    if (!is.logical(short_version) | is.na(short_version)) {

      stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

    }

    # join information and filter by threshold
    data <-
      tibble(candidacies, ballots) |>
      mutate("porc_ballots" = ballots/(sum(ballots) + first(blank_ballots)))

    data_filtered <-
      data |>
      filter(porc_ballots >= threshold)

    # quotients
    quotients <-
      (1:n_seats) |>
      map_dfr(function (x) {
        tibble("candidacies" = data_filtered$candidacies,
               "divisor" = x,
               "quotient" = (data_filtered |> pull(ballots)) / x)
      })

    # top quotients
    top_quotients <-
      quotients |>
      left_join(data, by = "candidacies") |>
      # in case of ties -> by ballots
      arrange(desc(quotient), desc(ballots)) |>
      slice(1:n_seats) |>
      select(candidacies, divisor, quotient)

    # seats
    seats <-
      top_quotients |>
      count(candidacies, name = "seats") |>
      arrange(desc(seats)) |>
      full_join(tibble(candidacies), by = "candidacies") |>
      mutate("seats" = if_else(is.na(seats), 0, seats)) |>
      full_join(data, by = "candidacies")

    if (short_version) {

      return(seats |> select(candidacies, seats))

    } else {

      # in a long format we include the quotients
      seats <-
        seats |>
        left_join(quotients |>
                    pivot_wider(names_from = divisor,
                                values_from = quotient),
                  by = "candidacies") |>
        set_names(c("candidacies", "seats", "ballots",
                    "porc_ballots", paste0("quotient_", 1:n_seats))) |>
        mutate("porc_seats" = 100*seats/sum(seats),
               "porc_ballots" = 100*porc_ballots,
               .after = ballots)

      return(seats)
    }
  }

#' @title Function to calculate the allocated seats according to
#' the Hamilton method in a given electoral district.
#'
#' @description This function allocates seats to political parties in a
#' given electoral district using the Hamilton method (also
#' known as the method of largest remainders).The method first
#' calculates an electoral quota by dividing the total number
#' of votes (including blank votes) by the number of seats to be filled.
#' Each party's vote count is divided by this quota to determine an initial seat
#' allocation (using the floor of the result). Remaining seats are then assigned
#' to candidacies with the largest fractional remainders until all seats are distributed.
#' Only parties that surpass a given vote threshold (expressed as a proportion of
#' total votes) are considered for seat allocation.
#'
#' @inheritParams dhondt_seats
#'
#' @returns A tibble or a list of tibbles with rows corresponding
#' to each party including the following variables:
#' \item{candidacies}{abbrev or id of the candidacies}
#' \item{seats}{number of seats}
#' \item{ballots}{absolute number of ballots, just in long format}
#' \item{porc_seats}{percentage of seats respect to the number of seats,
#' just in long format}
#' \item{porc_ballots}{percentage of ballots respect to party ballots
#' (including blank ballots), just in long format}
#' \item{remainder}{remainders of the initial division that were
#' not selected for a seat, just in long format}
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
#' ## Seats distribution with Hamilton method for given vectors of candidacies and ballots
#' ## without the remainder quotients
#'
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100, 200)
#'
#' seats <- hamilton_seats(candidacies = candidacies, ballots = ballots,
#' blank_ballots = 50, n_seats = 15, threshold = 0.03)
#'
#' \dontrun{
#'
#' # Incorrect examples
#'
#' # Different length of candidacies and ballots
#'
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100)
#'
#' seats <- hamilton_seats(candidacies = candidacies, ballots = ballots,
#' blank_ballots = 50, threshold = 0.03)
#' }
#' @export
hamilton_seats <- function(candidacies, ballots, blank_ballots, n_seats,
                           threshold = 0, short_version = TRUE) {

  if (length(candidacies) != length(ballots)) {

    stop(red("Ups! `candidacies` and `ballots` should have the same length"))

  }

  if (length(unique(candidacies)) != length(candidacies)) {

    stop(red("Ups! `candidacies` should contain unique values"))

  }

  if (any(!is.numeric(ballots) | is.na(ballots))) {

    stop(red("Ups! `ballots` should be a numeric vector without missing"))

  }

  if (any(ballots < 0) | any(floor(ballots) != ballots)) {

    stop(red("Ups! `ballots` should be a numeric vector with positive integer values"))

  }

  if (any(!is.numeric(blank_ballots))) {

    stop(red("Ups! `blank_ballots` should be a numeric varible"))

  }

  if (any(blank_ballots < 0) |
      any(floor(blank_ballots) != blank_ballots)) {

    stop(red("Ups! `blank_ballots` should be a numeric variable with positive integer values"))

  }

  if (length(unique(blank_ballots)) > 1) {

    stop(red("Ups! `blank_ballots` should be just a numeric value or a repeated numerical vector"))

  }

  if (any(!is.numeric(n_seats) | is.na(n_seats) | (length(n_seats) > 1))) {

    stop(red("Ups! `n_seats` should be a single numerical value"))

  }

  if (n_seats < 0 | (floor(n_seats) != n_seats)) {

    stop(red("Ups! `n_seats` should be an integer positive value"))

  }

  if (!is.numeric(threshold) | !between(threshold, 0, 1) |
      is.na(threshold) | (length(threshold) > 1)) {

    stop(red("Ups! `threshold` should be a single number between 0 and 1"))

  }

  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  data <-
    tibble(candidacies, ballots) |>
    mutate("porc_ballots" = ballots/(sum(ballots) + first(blank_ballots)))

  data_filtered <-
    data |>
    filter(porc_ballots >= threshold)

  data_filtered <- data_filtered |>
    mutate(
      exact_seats = ballots / ((sum(ballots) + first(blank_ballots)) / n_seats),
      # The dividend represents the quota
      initial_seats = floor(exact_seats),
      remainder = exact_seats - initial_seats
    )

  remaining_seats <- n_seats - sum(data_filtered$initial_seats)

  data_filtered <- data_filtered |>
    arrange(desc(remainder)) |>
    mutate(extra_seat = if_else(row_number() <= remaining_seats, 1, 0)) |>
    mutate(seats = initial_seats + extra_seat)

  seats <- data_filtered |>
    arrange(desc(seats)) |>
    full_join(tibble(candidacies), by = "candidacies") |>
    mutate("seats" = if_else(is.na(seats), 0, seats))

  if (short_version) {

    return(seats |> select(candidacies, seats))

  } else {

    seats <-
      seats |>
      mutate("porc_seats" = 100*seats/sum(seats),
             "porc_ballots" = 100*porc_ballots,
             .after = ballots)

    return(seats)
  }
}

#' @title Function to calculate the allocated seats according to the Webster method in a given
#' electoral district.
#'
#' @description This function allocates seats to political parties in a given electoral district
#' using the Webster method (also known as the Sainte-Laguë method), a highest averages
#' method for proportional representation. Each party's total number of ballots is divided
#' by a series of odd-numbered divisors (1, 3, 5, ...), generating a list of quotients.
#' The highest quotients are selected sequentially to assign the available seats.
#' Only parties that receive a number of ballots exceeding a specified threshold
#' (expressed as a proportion of the total ballots, including blank ballots) are eligible
#' for seat allocation.
#'
#' @inheritParams dhondt_seats
#'
#' @returns A tibble or a list of tibbles  with rows corresponding to each
#' party including the following variables:
#' \item{party}{aabbrev or id of the candidacies}
#' \item{seats}{number of seats}
#' \item{ballots}{absolute number of ballots, just in long format}
#' \item{porc_seats}{percentage of seats respect to the number of seats,
#' just in long format}
#' \item{porc_ballots}{percentage of ballots respect to party ballots
#' (including blank ballots), just in long format}
#' \item{quotient_x}{intermediate quotients of the allocation process}
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
#' ## Seats distribution with Hamilton method for given vectors of candidacies and ballots
#' ## without the remainder quotients
#'
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100, 200)
#'
#' seats <- webster_seats(candidacies = candidacies, ballots = ballots,
#' blank_ballots = 50, n_seats = 15, threshold = 0.03)
#'
#' \dontrun{
#'
#' # Incorrect examples
#'
#' # Different length of candidacies and ballots
#'
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100)
#'
#' seats <- webster_seats(candidacies = candidacies,
#' ballots = ballots, blank_ballots = 50, threshold = 0.03)
#'}
#' @export
webster_seats <- function(candidacies, ballots, blank_ballots, n_seats,
                          threshold = 0.03, short_version = TRUE) {

  if (length(candidacies) != length(ballots)) {

    stop(red("Ups! `candidacies` and `ballots` should have the same length"))

  }

  if (length(unique(candidacies)) != length(candidacies)) {

    stop(red("Ups! `candidacies` should contain unique values"))

  }

  if (any(!is.numeric(ballots) | is.na(ballots))) {

    stop(red("Ups! `ballots` should be a numeric vector without missing"))

  }

  if (any(ballots < 0) | any(floor(ballots) != ballots)) {

    stop(red("Ups! `ballots` should be a numeric vector with positive integer values"))

  }

  if (any(!is.numeric(blank_ballots))) {

    stop(red("Ups! `blank_ballots` should be a numeric varible"))

  }

  if (any(blank_ballots < 0) |
      any(floor(blank_ballots) != blank_ballots)) {

    stop(red("Ups! `blank_ballots` should be a numeric variable with positive integer values"))

  }

  if (length(unique(blank_ballots)) > 1) {

    stop(red("Ups! `blank_ballots` should be just a numeric value or a repeated numerical vector"))

  }

  if (any(!is.numeric(n_seats) | is.na(n_seats) | (length(n_seats) > 1))) {

    stop(red("Ups! `n_seats` should be a single numerical value"))

  }

  if (n_seats < 0 | (floor(n_seats) != n_seats)) {

    stop(red("Ups! `n_seats` should be an integer positive value"))

  }

  if (!is.numeric(threshold) | !between(threshold, 0, 1) |
      is.na(threshold) | (length(threshold) > 1)) {

    stop(red("Ups! `threshold` should be a single number between 0 and 1"))

  }

  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  data <-
    tibble(candidacies, ballots) |>
    mutate("porc_ballots" = ballots/(sum(ballots) + first(blank_ballots)))

  data_filtered <-
    data |>
    filter(porc_ballots >= threshold)

  quotients <- seq(1, 2 * n_seats - 1, by = 2) |>
    map_dfr(function (x) {tibble(
      "candidacies"    = data_filtered |> pull(candidacies),
      "divisor"  = x,
      "quotient" = (data_filtered |> pull(ballots)) / x)
    }
    )

  # top quotients
  top_quotients <-
    quotients |>
    left_join(data, by = "candidacies") |>
    # in case of ties -> by ballots
    arrange(desc(quotient), desc(ballots)) |>
    slice(1:n_seats) |>
    select(candidacies, divisor, quotient)

  # seats
  seats <-
    top_quotients |>
    count(candidacies, name = "seats") |>
    arrange(desc(seats)) |>
    full_join(tibble(candidacies), by = "candidacies") |>
    mutate("seats" = if_else(is.na(seats), 0, seats)) |>
    full_join(data, by = "candidacies")

  if (short_version) {

    return(seats |> select(candidacies, seats))

  } else {

    seats <-
      seats |>
      left_join(quotients |>
                  pivot_wider(names_from = divisor,
                              values_from = quotient),
                by = "candidacies") |>
      set_names(c("candidacies", "seats", "ballots",
                  "porc_ballots", paste0("quotient_", 1:n_seats))) |>
      mutate("porc_seats" = 100*seats/sum(seats),
             "porc_ballots" = 100*porc_ballots,
             .after = ballots)

    return(seats)
  }
}

#' @title Function to calculate the allocated seats according to the Hill
#' method in a given
#' electoral district.
#'
#' @description This function allocates seats to political parties in a given electoral district
#' using the Hill method (also known as the Equal Proportions method), a highest averages
#' approach for proportional representation. Each party's total number of votes is divided
#' by the square root of the product of the number of seats already allocated plus one and
#' that number itself (i.e., √n(n+1)), producing a list of quotients. Seats are assigned
#' one at a time to the party with the highest quotient, repeating until all seats are allocated.
#' Only candidacies surpassing a specified threshold (as a proportion of total votes including blank votes)
#' are considered for allocation.
#'
#' @inheritParams dhondt_seats
#'
#' @returns A tibble or a list of tibbles  with rows corresponding to each party including the following
#' variables:
#' \item{candidacies}{abbrev or id of the candidacies}
#' \item{seats}{number of seats}
#' \item{ballots}{absolute number of ballots, just in long format}
#' \item{porc_seats}{percentage of seats respect to the number of seats,
#' just in long format}
#' \item{porc_ballots}{percentage of ballots respect to party ballots
#' (including blank ballots), just in long format}
#' \item{quotient_x}{intermediate quotients of the allocation process}
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
#' ## Seats distribution with Hill method for given vectors of candidacies and ballots
#' ## without the remainder quotients
#'
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100, 200)
#'
#' seats <- hills_seats(candidacies = candidacies, ballots = ballots,
#'  blank_ballots = 50, n_seats = 15, threshold = 0.03)
#'
#' \dontrun{
#'
#' # Incorrect examples
#'
#' # Different length of candidacies and ballots
#'
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100)
#'
#' seats <- hills_seats(candidacies = candidacies, ballots = ballots,
#' blank_ballots = 50, threshold = 0.03)
#' }
#' @export
hills_seats <- function(candidacies, ballots, blank_ballots, n_seats,
                        threshold = 0.03, short_version = TRUE) {

  if (length(candidacies) != length(ballots)) {

    stop(red("Ups! `candidacies` and `ballots` should have the same length"))

  }

  if (length(unique(candidacies)) != length(candidacies)) {

    stop(red("Ups! `candidacies` should contain unique values"))

  }

  if (any(!is.numeric(ballots) | is.na(ballots))) {

    stop(red("Ups! `ballots` should be a numeric vector without missing"))

  }

  if (any(ballots < 0) | any(floor(ballots) != ballots)) {

    stop(red("Ups! `ballots` should be a numeric vector with positive integer values"))

  }

  if (any(!is.numeric(blank_ballots))) {

    stop(red("Ups! `blank_ballots` should be a numeric varible"))

  }

  if (any(blank_ballots < 0) |
      any(floor(blank_ballots) != blank_ballots)) {

    stop(red("Ups! `blank_ballots` should be a numeric variable with positive integer values"))

  }

  if (length(unique(blank_ballots)) > 1) {

    stop(red("Ups! `blank_ballots` should be just a numeric value or a repeated numerical vector"))

  }

  if (any(!is.numeric(n_seats) | is.na(n_seats) | (length(n_seats) > 1))) {

    stop(red("Ups! `n_seats` should be a single numerical value"))

  }

  if (n_seats < 0 | (floor(n_seats) != n_seats)) {

    stop(red("Ups! `n_seats` should be an integer positive value"))

  }

  if (!is.numeric(threshold) | !between(threshold, 0, 1) |
      is.na(threshold) | (length(threshold) > 1)) {

    stop(red("Ups! `threshold` should be a single number between 0 and 1"))

  }

  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  data <-
    tibble(candidacies, ballots) |>
    mutate("porc_ballots" = ballots/(sum(ballots) + first(blank_ballots)))

  data_filtered <-
    data |>
    filter(porc_ballots >= threshold)

  quotients <- map_dfr(
    1:n_seats,
    function (x) {tibble(
      "candidacies"    = data_filtered|> pull(candidacies),
      "divisor"  = sqrt(x * (x + 1)),
      "quotient" = data_filtered |> pull(ballots) / divisor)
    }
  )

  # top quotients
  top_quotients <-
    quotients |>
    left_join(data, by = "candidacies") |>
    # in case of ties -> by ballots
    arrange(desc(quotient), desc(ballots)) |>
    slice(1:n_seats) |>
    select(candidacies, divisor, quotient)

  # seats
  seats <-
    top_quotients |>
    count(candidacies, name = "seats") |>
    arrange(desc(seats)) |>
    full_join(tibble(candidacies), by = "candidacies") |>
    mutate("seats" = if_else(is.na(seats), 0, seats)) |>
    full_join(data, by = "candidacies")


  if (short_version) {

    return(seats |>  select(candidacies, seats))

  } else {

    seats <-
      seats |>
      left_join(quotients |>
                  pivot_wider(names_from = divisor,
                              values_from = quotient),
                by = "candidacies") |>
      set_names(c("candidacies", "seats", "ballots",
                  "porc_ballots", paste0("quotient_", 1:n_seats))) |>
      mutate("porc_seats" = 100*seats/sum(seats),
             "porc_ballots" = 100*porc_ballots,
             .after = ballots)

    return(seats)
  }
}

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
#' @inheritParams dhondt_seats
#'
#' @returns A tibble or a list of tibbles  with rows corresponding to each party
#' including the following variables:
#' \item{candidacies}{abbrev or id of the candidacies}
#' \item{seats}{number of seats}
#' \item{ballots}{absolute number of ballots, just in long format}
#' \item{porc_seats}{percentage of seats respect to the number of seats,
#' just in long format}
#' \item{porc_ballots}{percentage of ballots respect to party ballots
#' (including blank ballots), just in long format}
#' \item{quotient_x}{intermediate quotients of the allocation process}
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
#' ## Seats distribution with Dean method for given vectors of candidacies and ballots
#' ## without the remainder quotients
#'
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100, 200)
#'
#' seats <- deans_seats(candidacies = candidacies, ballots = ballots,
#' blank_ballots = 50, n_seats = 15, threshold = 0.03)
#'
#' \dontrun{
#'
#' # Incorrect examples
#'
#' # Different length of candidacies and ballots
#'
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100)
#'
#' seats <- deans_seats(candidacies = candidacies, ballots = ballots,
#' blank_ballots = 50, threshold = 0.03)
#'}
#' @export
deans_seats <- function(candidacies, ballots, blank_ballots, n_seats,
                        threshold = 0.03, short_version = TRUE) {

  if (length(candidacies) != length(ballots)) {

    stop(red("Ups! `candidacies` and `ballots` should have the same length"))

  }

  if (length(unique(candidacies)) != length(candidacies)) {

    stop(red("Ups! `candidacies` should contain unique values"))

  }

  if (any(!is.numeric(ballots) | is.na(ballots))) {

    stop(red("Ups! `ballots` should be a numeric vector without missing"))

  }

  if (any(ballots < 0) | any(floor(ballots) != ballots)) {

    stop(red("Ups! `ballots` should be a numeric vector with positive integer values"))

  }

  if (any(!is.numeric(blank_ballots))) {

    stop(red("Ups! `blank_ballots` should be a numeric varible"))

  }

  if (any(blank_ballots < 0) |
      any(floor(blank_ballots) != blank_ballots)) {

    stop(red("Ups! `blank_ballots` should be a numeric variable with positive integer values"))

  }

  if (length(unique(blank_ballots)) > 1) {

    stop(red("Ups! `blank_ballots` should be just a numeric value or a repeated numerical vector"))

  }

  if (any(!is.numeric(n_seats) | is.na(n_seats) | (length(n_seats) > 1))) {

    stop(red("Ups! `n_seats` should be a single numerical value"))

  }

  if (n_seats < 0 | (floor(n_seats) != n_seats)) {

    stop(red("Ups! `n_seats` should be an integer positive value"))

  }

  if (!is.numeric(threshold) | !between(threshold, 0, 1) |
      is.na(threshold) | (length(threshold) > 1)) {

    stop(red("Ups! `threshold` should be a single number between 0 and 1"))

  }

  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  data <-
    tibble(candidacies, ballots) |>
    mutate("porc_ballots" = ballots/(sum(ballots) + first(blank_ballots)))

  data_filtered <-
    data |>
    filter(porc_ballots >= threshold)

  divisor_dean <- function(s)
    if_else(s == 0, 1,
            (2 * s * (s + 1)) / (2 * s + 1))

  divisors <- map_dbl(0:(n_seats - 1), divisor_dean)

  quotients <- pmap_dfr(
    list(candidacies = data_filtered$candidacies, v = data_filtered$ballots),
    function(candidacies, v) {
      tibble(
        candidacies = candidacies,
        divisor  = divisors,
        quotient = v / divisors
      )
    }
  )

  # top quotients
  top_quotients <-
    quotients |>
    left_join(data, by = "candidacies") |>
    # in case of ties -> by ballots
    arrange(desc(quotient), desc(ballots)) |>
    slice(1:n_seats) |>
    select(candidacies, divisor, quotient)

  # seats
  seats <-
    top_quotients |>
    count(candidacies, name = "seats") |>
    arrange(desc(seats)) |>
    full_join(tibble(candidacies), by = "candidacies") |>
    mutate("seats" = if_else(is.na(seats), 0, seats)) |>
    full_join(data, by = "candidacies")



  if (short_version) {

    return(seats |> select(candidacies, seats))

  } else {

    seats <-
      seats |>
      left_join(quotients |>
                  pivot_wider(names_from = divisor,
                              values_from = quotient),
                by = "candidacies") |>
      set_names(c("candidacies", "seats", "ballots",
                  "porc_ballots", paste0("quotient_", 1:n_seats))) |>
      mutate("porc_seats" = 100*seats/sum(seats),
             "porc_ballots" = 100*porc_ballots,
             .after = ballots)

    return(seats)
  }
}

#' @title Function to calculate the allocated seats according to the Adams method in a given
#' electoral district.
#'
#' @description This function allocates seats to political parties in a given electoral district
#' using the Adams method, a highest averages method for proportional representation
#' that favors smaller candidacies. In this method, each party’s vote total is divided
#' by a sequence of integers starting from 1 (i.e., 1, 2, 3, ...), generating a series
#' of quotients. Seats are allocated one at a time to the highest quotients until all
#' seats are distributed. Only parties that surpass a specified vote threshold
#' (expressed as a proportion of total votes, including blank votes) are eligible
#' for seat allocation.
#'
#' @inheritParams dhondt_seats
#'
#' @returns A tibble or a list of tibbles  with rows corresponding to each party including the following
#' variables:
#' \item{candidacies}{abbrev or id of the candidacies}
#' \item{seats}{number of seats distributed to each party}
#' \item{ballots}{absolute number of ballots, just in long format}
#' \item{porc_seats}{percentage of seats respect to the number of seats,
#' just in long format}
#' \item{porc_ballots}{percentage of ballots respect to party ballots
#' (including blank ballots), just in long format}
#' \item{quotient_x}{intermediate quotients of the allocation process}
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
#' ## Seats distribution with Adams method for given vectors of candidacies and ballots
#' ## without the remainder quotients
#'
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100, 200)
#'
#' seats <- adams_seats(candidacies = candidacies, ballots = ballots,
#' blank_ballots = 50, n_seats = 15, threshold = 0.03)
#'
#' \dontrun{
#'
#' # Incorrect examples
#'
#' # Different length of candidacies and ballots
#'
#' candidacies <- c("PP", "PSOE", "PODEMOS", "VOX")
#' ballots <- c(200, 350, 100)
#'
#' seats <- adams_seats(candidacies = candidacies, ballots = ballots,
#'  blank_ballots = 50, threshold = 0.03)
#'}
#' @export
adams_seats <- function(candidacies, ballots, blank_ballots, n_seats,
                        threshold = 0.03, short_version = TRUE) {

  if (length(candidacies) != length(ballots)) {

    stop(red("Ups! `candidacies` and `ballots` should have the same length"))

  }

  if (length(unique(candidacies)) != length(candidacies)) {

    stop(red("Ups! `candidacies` should contain unique values"))

  }

  if (any(!is.numeric(ballots) | is.na(ballots))) {

    stop(red("Ups! `ballots` should be a numeric vector without missing"))

  }

  if (any(ballots < 0) | any(floor(ballots) != ballots)) {

    stop(red("Ups! `ballots` should be a numeric vector with positive integer values"))

  }

  if (any(!is.numeric(blank_ballots))) {

    stop(red("Ups! `blank_ballots` should be a numeric varible"))

  }

  if (any(blank_ballots < 0) |
      any(floor(blank_ballots) != blank_ballots)) {

    stop(red("Ups! `blank_ballots` should be a numeric variable with positive integer values"))

  }

  if (length(unique(blank_ballots)) > 1) {

    stop(red("Ups! `blank_ballots` should be just a numeric value or a repeated numerical vector"))

  }

  if (any(!is.numeric(n_seats) | is.na(n_seats) | (length(n_seats) > 1))) {

    stop(red("Ups! `n_seats` should be a single numerical value"))

  }

  if (n_seats < 0 | (floor(n_seats) != n_seats)) {

    stop(red("Ups! `n_seats` should be an integer positive value"))

  }

  if (!is.numeric(threshold) | !between(threshold, 0, 1) |
      is.na(threshold) | (length(threshold) > 1)) {

    stop(red("Ups! `threshold` should be a single number between 0 and 1"))

  }

  if (!is.logical(short_version) | is.na(short_version)) {

    stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

  }

  data <-
    tibble(candidacies, ballots) |>
    mutate("porc_ballots" = ballots/(sum(ballots) + first(blank_ballots)))

  data_filtered <-
    data |>
    filter(porc_ballots >= threshold)

  initial_seats <- data_filtered |>
    arrange(desc(ballots)) |>
    mutate("initial_seats" = if_else(row_number() <= n_seats, 1, NA))

  remaining_seats <- n_seats - nrow(initial_seats)

  if (remaining_seats <= 0) {

    if (short_version){

    initial_seats <- initial_seats |>
      full_join(tibble(candidacies), by = "candidacies") |>
      mutate("seats" = if_else(is.na(initial_seats), 0, initial_seats))

    return(initial_seats |> select(candidacies, seats))

    } else {

      initial_seats <- initial_seats |>
        full_join(tibble(candidacies), by = "candidacies") |>
        mutate("seats" = if_else(is.na(initial_seats), 0, initial_seats)) |>
        mutate("porc_seats" = 100*seats/sum(seats),
               "porc_ballots" = 100*porc_ballots,
               .after = ballots)

      return(initial_seats |> select(-initial_seats))

    }

  }

  divisors <- 1:remaining_seats

  quotients <- pmap_dfr(
    list(candidacies = data_filtered$candidacies, v = data_filtered$ballots),
    function(candidacies, v) {
      tibble(
        candidacies    = candidacies,
        divisor  = divisors,
        quotient = v / divisor
      )
    }
  )

  top_quotients <-
    quotients |>
    left_join(data, by = "candidacies") |>
    # in case of ties -> by ballots
    arrange(desc(quotient), desc(ballots)) |>
    slice(1:remaining_seats) |>
    select(candidacies, divisor, quotient)

  extra_seats <- top_quotients |>
    count(candidacies, name = "extra")

  seats <- initial_seats |>
    left_join(extra_seats, by = "candidacies") |>
    mutate(extra_seats = replace_na(extra, 0),
           seats = initial_seats + extra_seats) |>
    arrange(desc(seats)) |>
    full_join(tibble(candidacies), by = "candidacies") |>
    mutate("seats" = if_else(is.na(seats), 0, seats)) |>
    full_join(data, by = "candidacies") |>
    select(-c(ballots.x, porc_ballots.x)) |>
    relocate(c(ballots.y, porc_ballots.y), .after = candidacies) |>
    rename("ballots" = "ballots.y",
           "porc_ballots" = "porc_ballots.y")

  if (short_version) {

    return(seats |>  select(candidacies, seats))

  } else {

    seats <-
      seats |>
      left_join(quotients |>
                  pivot_wider(names_from = divisor,
                              values_from = quotient),
                by = "candidacies") |>
      select(-c(extra, initial_seats, extra_seats)) |>
      set_names(c("candidacies", "ballots",
                  "porc_ballots", "seats", paste0("quotient_", 1:remaining_seats))) |>
      mutate("porc_seats" = 100*seats/sum(seats),
             "porc_ballots" = 100*porc_ballots,
             .after = ballots)

    return(seats)
  }
}

#' @title Function to compute the allocated seats according to the
#' chosen method for a given electoral districts.
#'
#' @inheritParams dhondt_seats
#' @param method A string vector providing the methods of
#' apportionment to be used. The allowed values are the following:
#' \code{"D'Hondt"} (or \code{"Hondt"} or \code{"hondt"}),
#' \code{"Hamilton"} (or \code{"hamilton"} or \code{"Vinton"} or
#' \code{"vinton"}), \code{"Webster"} (or \code{"webster"} or
#' \code{"Sainte-Lague"} or \code{"sainte-lague"}), \code{"Hill"} (or
#' \code{"hill"} or \code{"Huntington-Hill"} or
#' \code{"huntington-hill"}), \code{"Dean"} (or \code{"dean"}) and
#' \code{"Adams"} (or \code{"adams"}) . Defaults to \code{"Hondt"}.
#'
#' @returns A tibble with rows corresponding to each party including
#' the following variables:
#' \item{candidacies}{abbrev or id of the candidacies}
#' \item{seats}{number of seats}
#' \item{method}{method to allocate seats}
#' \item{ballots}{absolute number of ballots, just in long format}
#' \item{porc_ballots}{percentage of ballots respect to party ballots
#' (including blank ballots), just in long format}
#' \item{quotient_x}{intermediate quotients of the allocation process}
#'
#' @details The purpose of this function is to calculate the
#' allocation of seats for a given electoral district (and given a
#' particular election) for a set of apportionment methods.
#'
#' @author David Pereiro-Pol, Irene Bosque-Gala and Javier
#' Alvarez-Liebana.
#' @keywords seat_allocation
#' @name seat_allocation
#' @import crayon
#'
#' @examples
#'
#' ##Correct examples
#'
#' \dontrun{
#'
#' # ----
#' # Incorrect examples
#' # ----
#'
#' }
#' @export
seat_allocation <-
  function(candidacies, ballots, blank_ballots, n_seats,
           method = "hondt", threshold = 0.03, short_version = TRUE) {

    if (length(candidacies) != length(ballots)) {

      stop(red("Ups! `candidacies` and `ballots` should have the same length"))

    }

    if (length(unique(candidacies)) != length(candidacies)) {

      stop(red("Ups! `candidacies` should contain unique values"))

    }

    if (any(!is.numeric(ballots) | is.na(ballots))) {

      stop(red("Ups! `ballots` should be a numeric vector without missing"))

    }

    if (any(ballots < 0) | any(floor(ballots) != ballots)) {

      stop(red("Ups! `ballots` should be a numeric vector with positive integer values"))

    }

    if (any(!is.numeric(blank_ballots))) {

      stop(red("Ups! `blank_ballots` should be a numeric varible"))

    }

    if (any(blank_ballots < 0) |
        any(floor(blank_ballots) != blank_ballots)) {

      stop(red("Ups! `blank_ballots` should be a numeric variable with positive integer values"))

    }

    if (length(unique(blank_ballots)) > 1) {

      stop(red("Ups! `blank_ballots` should be just a numeric value or a repeated numerical vector"))

    }

    if (any(!is.numeric(n_seats) | is.na(n_seats) | (length(n_seats) > 1))) {

      stop(red("Ups! `n_seats` should be a single numerical value"))

    }

    if (n_seats < 0 | (floor(n_seats) != n_seats)) {

      stop(red("Ups! `n_seats` should be an integer positive value"))

    }

    if (!is.numeric(threshold) | !between(threshold, 0, 1) |
        is.na(threshold) | (length(threshold) > 1)) {

      stop(red("Ups! `threshold` should be a single number between 0 and 1"))

    }

    if (!is.logical(short_version) | is.na(short_version)) {

      stop(red("Ups! `short_version` argument should be a TRUE/FALSE variable."))

    }

    if (!is.character(method) | any(is.na(method))) {

      stop(red("Ups! `method` argument should be a string vector without missing values"))

    }

    method <- str_to_lower(method)
    method <-
      if_else(method %in% c("d'hondt", "hondt"), "hondt",
              if_else(method %in% c("hamilton", "vinton"), "hamilton",
                      if_else(method %in% c("webster", "sainte-lague"), "hamilton",
                              if_else(method %in% c("hill", "huntington-hill"), "hill",
                                      if_else(method %in% c("dean"), "dean",
                                              if_else(method %in% c("adams"), "adams", method))))))
    method <- unique(method)

    apportion_fun <-
      method |>
      map(function(x) {
        switch(x,
               "hondt" = dhondt_seats, "hamilton" = hamilton_seats,
               "webster" = webster_seats, "hill" = hill_seats,
               "dean" = deans_seats, "adams" = adams_seats) })

    if (any(apportion_fun |> map_lgl(function(x) { is.null(x) }))) {

      stop(red("Ups! `method` argument should be one of the following options: 'D'Hondt', 'd'hondt', 'Hondt', 'hondt', 'Hamilton', 'hamilton', 'Vinton', 'vinton', 'Webster', 'webster', 'sainte-lague', 'Sainte-Lague', 'Hill', 'hill', 'Huntington-Hill', 'huntington-hill', 'Dean', 'dean', 'Adams', 'adams'"))

    }

    seats_results <-
      apportion_fun |>
      map2_dfr(method, function(x, y) {
        x(candidacies = candidacies, ballots = ballots,
          blank_ballots = blank_ballots, n_seats = n_seats,
          threshold = threshold, short_version = short_version) |>
          mutate("method" = y)}) |>
      relocate(method, .after = candidacies)

    return(seats_results)
  }
