# ----- by_parties = FALSE -----

# all vs ccaa
by_all <-
  get_elections_data(type_elec = c("congress"), year = c(2019),
                     month = c(4), include_candidacies = TRUE,
                     include_candidates = FALSE) %>%
  aggregate_election_data(level = "all", by_parties = FALSE) %>%
  select(pop_res, n_poll_stations, census_counting,
         ballots_1, ballots_2, blank_ballots, invalid_ballots, party_ballots,
         valid_ballots, total_ballots, total_ballots_cera)
by_ccaa <-
  get_elections_data(type_elec = c("congress"), year = c(2019),
                     month = c(4), include_candidacies = TRUE,
                     include_candidates = FALSE) %>%
  aggregate_election_data(level = "ccaa", by_parties = FALSE) %>%
  select(pop_res, n_poll_stations, census_counting,
         ballots_1, ballots_2, blank_ballots, invalid_ballots, party_ballots,
         valid_ballots, total_ballots, total_ballots_cera)
all_equal(by_all, by_ccaa %>% summarise(across(everything(), sum)),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)


# all vs prov
# ccaa vs prov
by_prov <-
  get_elections_data(type_elec = c("congress"), year = c(2019),
                     month = c(4), include_candidacies = TRUE,
                     include_candidates = FALSE) %>%
  aggregate_election_data(level = "prov", by_parties = FALSE) %>%
  select(pop_res, n_poll_stations, census_counting,
         ballots_1, ballots_2, blank_ballots, invalid_ballots, party_ballots,
         valid_ballots, total_ballots, total_ballots_cera)
all_equal(by_all, by_prov %>% summarise(across(everything(), sum)),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
all_equal(by_ccaa %>% summarise(across(everything(), sum)),
          by_prov %>% summarise(across(everything(), sum)),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)

# all vs mun
# ccaa vs mun
# prov vs mun
by_mun <-
  get_elections_data(type_elec = c("congress"), year = c(2019),
                     month = c(4), include_candidacies = TRUE,
                     include_candidates = FALSE) %>%
  aggregate_election_data(level = "mun", by_parties = FALSE) %>%
  select(pop_res, n_poll_stations, census_counting,
         ballots_1, ballots_2, blank_ballots, invalid_ballots, party_ballots,
         valid_ballots, total_ballots, total_ballots_cera)
all_equal(by_all,
          by_mun %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
all_equal(by_ccaa %>% summarise(across(everything(), sum)),
          by_mun %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
all_equal(by_prov %>% summarise(across(everything(), sum)),
          by_mun %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)


# all vs mun-district
# ccaa vs mun-district
# prov vs mun-district
# mun vs mun-district
by_mun_district <-
  get_elections_data(type_elec = c("congress"), year = c(2019),
                     month = c(4), include_candidacies = TRUE,
                     include_candidates = FALSE) %>%
  aggregate_election_data(level = "mun-district", by_parties = FALSE) %>%
  select(n_poll_stations, census_counting,
         ballots_1, ballots_2, blank_ballots, invalid_ballots, party_ballots,
         valid_ballots, total_ballots, total_ballots_cera)
all_equal(by_all %>% select(-pop_res),
          by_mun_district %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
all_equal(by_ccaa %>% select(-pop_res) %>% summarise(across(everything(), sum)),
          by_mun_district %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
all_equal(by_prov %>% select(-pop_res) %>% summarise(across(everything(), sum)),
          by_mun_district %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
all_equal(by_mun %>% select(-pop_res) %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          by_mun_district %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)

# all vs sec
# ccaa vs sec
# prov vs sec
# mun vs sec
# mun-district vs sec
by_sec <-
  get_elections_data(type_elec = c("congress"), year = c(2019),
                     month = c(4), include_candidacies = TRUE,
                     include_candidates = FALSE) %>%
  aggregate_election_data(level = "sec", by_parties = FALSE) %>%
  select(n_poll_stations, census_counting,
         ballots_1, ballots_2, blank_ballots, invalid_ballots, party_ballots,
         valid_ballots, total_ballots, total_ballots_cera)
all_equal(by_all %>% select(-pop_res),
          by_sec %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
all_equal(by_ccaa %>% select(-pop_res) %>% summarise(across(everything(), sum)),
          by_sec %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
all_equal(by_prov %>% select(-pop_res) %>% summarise(across(everything(), sum)),
          by_sec %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
all_equal(by_mun %>% select(-pop_res) %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          by_sec %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
all_equal(by_mun_district %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          by_sec %>% summarise(across(everything(), function(x) { sum(x, na.rm = TRUE) })),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)

# ----- by_parties = TRUE -----

# all vs ccaa
by_all <-
  get_elections_data(type_elec = c("congress"), year = c(2019),
                     month = c(4), include_candidacies = TRUE,
                     include_candidates = FALSE) %>%
  aggregate_election_data(level = "all", by_parties = TRUE) %>%
  select(id_candidacies, ballots, elected)
by_ccaa <-
  get_elections_data(type_elec = c("congress"), year = c(2019),
                     month = c(4), include_candidacies = TRUE,
                     include_candidates = FALSE) %>%
  aggregate_election_data(level = "ccaa", by_parties = TRUE)  %>%
  select(id_candidacies, ballots, elected)
all_equal(by_all,
          by_ccaa %>%
            group_by(id_candidacies) %>%
            summarise(across(ballots:elected, sum)) %>%
            ungroup(),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)


# all vs prov
# ccaa vs prov
by_prov <-
  get_elections_data(type_elec = c("congress"), year = c(2019),
                     month = c(4), include_candidacies = TRUE,
                     include_candidates = FALSE) %>%
  aggregate_election_data(level = "prov", by_parties = TRUE) %>%
  select(id_candidacies, ballots, elected)
all_equal(by_all,
          by_prov %>%
            group_by(id_candidacies) %>%
            summarise(across(ballots:elected, sum)) %>%
            ungroup(),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
all_equal(by_ccaa %>%
            group_by(id_candidacies) %>%
            summarise(across(ballots:elected, sum)) %>%
            ungroup(),
          by_prov %>%
            group_by(id_candidacies) %>%
            summarise(across(ballots:elected, sum)) %>%
            ungroup(),
          ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
