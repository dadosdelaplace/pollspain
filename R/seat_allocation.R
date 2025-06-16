
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
#' # parties and votes without the remainder quotients
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

    stop(red("Ups! `parties` and `ballots` should have the same length"))

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

      stop(red("Ups! `parties` and `ballots` should have the same length"))

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
