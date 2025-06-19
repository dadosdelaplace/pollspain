# Script whose sole purpose is to prevent devtools::check() from
# issuing a NOTE due to the use of tidyverse and lazy-eval functions
# inside other functions.
utils::globalVariables(c(
  ".data", ":=", "ballots", "matches", "any_of", "everything", "id_candidacies",
  "id_candidacies_nat", "porc_ballots", "desc", "quotient", "divisor", "dates_elections_spain",
  "cod_elec", "party_ballots", "sum_party_ballots", "blank_ballots", "valid_ballots",
  "invalid_ballots", "total_ballots", "census_counting_mun", "ccaa", "prov", "mun", "turnout",
  "porc_valid", "porc_invalid", "porc_parties", "porc_blank", "pop_res_mun", "date_elec",
  "cod_INE_prov", "cod_INE_mun", "cd_INE_mun", "turn", "id_INE_mun", "id_elec", "id_INE_poll_station",
  "id_MIR_mun", "id_candidacies_ccaa", "abbrev_candidacies", "name_candidacies", "n_poll_stations",
  "census_CERE_mun", "cod_mun_district", "cod_sec", "cod_poll_station", "cod_MIR_ccaa",
  "cod_INE_ccaa", "cod_INE_ccaa.y", "ccaa.y", "prov.y", "census_INE_mun", "ballots_1", "ballots_2",
  "turnout_1", "turnout_2", "cod_mun_jud_district", "cod_mun_prov_council", "hamilton_seats",
  "webster_seats", "hill_seats", "deans_seats", "adams_seats", "global_dict_parties", "color",
  "name_candidacies_nat", "porc_candidacies_valid", "all_of", "'voters_cere'", "voters_cere"
))
