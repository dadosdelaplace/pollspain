

dhondt_seats <- function(parties, votes, nseats, threshold, short_version = TRUE) {
  total_votes <- sum(votes)
  threshold_votes <- threshold * total_votes

  data <- tibble(party = parties, votes = votes) |>
    filter(party != "blank", votes > threshold_votes)


  quotients <- tibble()
  for (i in 1:nseats) {
    quotients <- bind_rows(
      quotients,
      tibble(party = data$party, divisor = i, quotient = data$votes / i)
    )
  }

  top_quotients <- quotients |> slice_max(quotient, n = nseats, with_ties = FALSE)


  seats <- top_quotients |>
    count(party, name = "seats") |>
    arrange(desc(seats))


  if (short_version) {
    return(seats)
  } else {
    # Cocientes no utilizados (restos) usando anti_join
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

hamilton_seats <- function(parties, votes, nseats, threshold, short_version = TRUE) {
  total_votes <- sum(votes)
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

webster_seats <- function(parties, votes, nseats, threshold, short_version = TRUE) {
  total_votes <- sum(votes)
  threshold_votes <- threshold * total_votes

  data <- tibble(party = parties, votes = votes) |>
    filter(party != "blank", votes > threshold_votes)

  quotients <- tibble()
  for (i in seq(1, 2 * nseats - 1, by = 2)) {
    quotients <- bind_rows(
      quotients,
      tibble(party = data$party, quotient = data$votes / i)
    )
  }

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

hills_seats <- function(parties, votes, nseats, threshold, short_version = TRUE) {
  total_votes <- sum(votes)
  threshold_votes <- threshold * total_votes

  data <- tibble(party = parties, votes = votes) |>
    filter(party != "blank", votes > threshold_votes)

  quotients <- tibble()

  for (i in 1:nseats) {
    quotients <- bind_rows(
      quotients,
      tibble(party = data$party, quotient = data$votes / (i + 1))
    )
  }


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

deans_seats <- function(parties, votes, nseats, threshold, short_version = TRUE) {
  total_votes <- sum(votes)
  threshold_votes <- threshold * total_votes

  data <- tibble(party = parties, votes = votes) |>
    filter(party != "blank", votes > threshold_votes) |>
    mutate(seats = 0)

  allocations <- tibble()

  for (i in 1:nseats) {
    data <- data |>
      mutate(
        divisor = if_else(seats == 0, 1, (2 * seats * (seats + 1)) / (2 * seats + 1)),
        quotient = votes / divisor
      )

    winner <- data |> slice_max(quotient, n = 1, with_ties = FALSE)

    allocations <- bind_rows(allocations, winner |> select(party, quotient))

    data <- data |>
      mutate(seats = if_else(party == winner$party, seats + 1, seats))
  }

  seats <- data |> select(party, seats) |> arrange(desc(seats))


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

adams_seats <- function(parties, votes, nseats, threshold, short_version = TRUE) {
  total_votes <- sum(votes)
  threshold_votes <- threshold * total_votes

  data <- tibble(party = parties, votes = votes) |>
    filter(party != "blank", votes > threshold_votes) |>
    mutate(seats = 0)

  allocations <- tibble()

  for (i in 1:nseats) {
    data <- data |>
      mutate(
        divisor = seats + 1,
        quotient = votes / divisor
      )

    winner <- data |> slice_max(quotient, n = 1, with_ties = FALSE)

    allocations <- bind_rows(allocations, winner |> select(party, quotient))

    data <- data |>
      mutate(seats = if_else(party == winner$party, seats + 1, seats))
  }

  seats <- data |> select(party, seats) |> arrange(desc(seats))

  if (short_version) {
    return(seats)
  } else {

    all_quotients <- tibble()
    for (i in 1:nseats) {
      all_quotients <- bind_rows(
        all_quotients,
        tibble(party = data$party, quotient = data$votes / (data$seats + 1))
      )
    }

    remainders <- anti_join(all_quotients, allocations, by = c("party", "quotient")) |>
      select(party, quotient) |>
      arrange(desc(quotient))

    return(list(
      seats = seats,
      remainders = remainders
    ))
  }
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
