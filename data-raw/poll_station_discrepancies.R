fechas <- dates_elections_spain |> filter(type_elec == "congress") |> pull(date)
errors <- tibble()
for (i in 1:length(fechas)) {

  poll_data <-
    import_poll_station_data(type_elec = "congress", date = fechas[i])
  candidacies_data <-
    import_candidacies_data(type_elec = "congress", date = fechas[i])

  errors <-
    errors |>
    bind_rows(tibble("date" = fechas[i],
                     poll_data |>
    left_join(candidacies_data |>
                summarise("party_ballots_cand" = sum(ballots), .by = id_INE_poll_station),
              by = "id_INE_poll_station", suffix = c("", ".y")) |>
    filter(party_ballots != party_ballots_cand) |>
    select(id_INE_poll_station, party_ballots, party_ballots_cand)))
}


errors <-
  errors |>
  mutate("diff" = party_ballots - party_ballots_cand) |>
  rename(party_ballots_09_DAT = party_ballots,
         party_ballots_10_DAT = party_ballots_cand)
