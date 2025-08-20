
# ----- packages -----
library(tidyverse)
library(glue)
library(lubridate)
library(readr)


library(tibble)

seats_1982 <- tibble(
  prov = c(
    "Madrid","Pontevedra","La Coruña/A Coruña","Lugo","Orense/Ourense",
    "Murcia","Barcelona","Girona","Tarragona","Lleida"
  ),
  PSOE        = c(18, 3, 4, 1, 1, 5, 18, 2, 3, 2),
  `AP-PDP-PL` = c(11, 4, 4, 3, 2, 3, 5, 1, 1, 1),
  CIU         = c( 0, 0, 0, 0, 0, 0, 8, 2, 1, 1),
  PSUC        = c( 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  UCD         = c( 1, 1, 1, 1, 2, 0, 0, 0, 0, 0),
  CDS         = c( 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  PCE         = c( 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  ERC         = c( 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),

  id_elec = "02-1982-10-28"
)

seats_1986 <- tibble(
  prov = c(
    "Madrid","Pontevedra","La Coruña/A Coruña","Lugo","Orense/Ourense",
    "Murcia","Barcelona","Girona","Tarragona","Lleida"
  ),
  PSOE        = c(15, 3, 4, 2, 2, 5, 16, 2, 2, 1),
  `AP-PDP-PL` = c(11, 4, 4, 3, 2, 3, 4, 0, 1, 1),
  CIU         = c( 0, 0, 0, 0, 0, 0, 11, 3, 2, 2),
  IU          = c( 2, 0, 0, 0, 0, 0,  0, 0, 0, 0),
  `PSUC-ENE`  = c( 0, 0, 0, 0, 0, 0,  1, 0, 0, 0),
  CDS         = c( 5, 1, 1, 0, 0, 0,  1, 0, 0, 0),
  CG          = c( 0, 0, 0, 0, 1, 0,  0, 0, 0, 0),

  id_elec = "02-1986-06-22"
)

seats_1989 <- tibble(
  prov = c(
    "Madrid","Pontevedra","La Coruña/A Coruña","Lugo","Orense/Ourense",
    "Murcia","Barcelona","Girona","Tarragona","Lleida"
  ),
  PSOE = c(12, 4, 4, 2, 2, 5, 14, 2, 2, 2),
  PP   = c(12, 4, 4, 3, 3, 3,  3, 0, 1, 0),
  CIU  = c( 0, 0, 0, 0, 0, 0, 11, 3, 2, 2),
  IU   = c( 5, 0, 0, 0, 0, 0,  3, 0, 0, 0),
  CDS  = c( 4, 0, 1, 0, 0, 1,  1, 0, 0, 0),

  id_elec = "02-1989-10-29"
)

seats_1993 <- tibble(
  prov = c(
    "Madrid","Pontevedra","La Coruña/A Coruña","Lugo","Orense/Ourense",
    "Murcia","Barcelona","Girona","Tarragona","Lleida"
  ),

  PSOE = c(13, 3, 4, 2, 2, 4, 12, 2, 3, 1),
  PP   = c(16, 5, 5, 3, 2, 4,  6, 0, 1, 1),
  CIU  = c( 0, 0, 0, 0, 0, 0, 10, 3, 2, 2),
  IU   = c( 5, 0, 0, 0, 0, 1,  3, 0, 0, 0),
  CDS  = c( 0, 0, 0, 0, 0, 0,  0, 0, 0, 0),
  ERC  = c( 0, 0, 0, 0, 0, 0,  1, 0, 0, 0),

  id_elec = "02-1993-06-06"
)

seats_1996 <- tibble(
  prov = c(
    "Madrid", "Pontevedra", "La Coruña/A Coruña", "Lugo", "Orense/Ourense",
    "Murcia", "Barcelona", "Girona", "Tarragona", "Lleida"
  ),

  PSOE = c(11, 3, 3, 1, 2, 3, 13, 2, 3, 1),
  PP   = c(17, 4, 5, 3, 2, 5, 6, 0, 1, 1),
  IU   = c(6,  0, 0, 0, 0, 1, 2, 0, 0, 0),
  CIU  = c(0,  0, 0, 0, 0, 0, 9, 3, 2, 2),
  ERC  = c(0,  0, 0, 0, 0, 0, 1, 0, 0, 0),
  BNG  = c(0,  1, 1, 0, 0, 0, 0, 0, 0, 0),

  id_elec = "02-1996-03-03"
)

seats_2000 <- tibble(
  prov = c(
    "Madrid", "Pontevedra", "La Coruña/A Coruña", "Lugo", "Orense/Ourense",
    "Murcia", "Barcelona", "Girona", "Tarragona", "Lleida"
  ),

  PSOE = c(12, 2, 2, 1, 1, 3, 12, 2, 2, 1),
  PP   = c(19, 5, 5, 3, 3, 6, 8,  1, 2, 1),
  IU   = c(3,  0, 0, 0, 0, 0, 0,  0, 0, 0),
  CIU  = c(0,  0, 0, 0, 0, 0, 9,  2, 2, 2),
  ERC  = c(0,  0, 0, 0, 0, 0, 1,  0, 0, 0),
  BNG  = c(0,  1, 2, 0, 0, 0, 0,  0, 0, 0),
  ICV  = c(0,  0, 0, 0, 0, 0, 1,  0, 0, 0),

  id_elec = "02-2000-03-12"
)

seats_2004 <- tibble(
  prov = c(
    "Madrid","Pontevedra","La Coruña/A Coruña","Lugo","Orense/Ourense",
    "Murcia","Barcelona","Girona","Tarragona","Lleida"
  ),

  PSOE = c(16,3,4,2,1,3,14,2,3,2),
  PP   = c(17,3,4,2,3,6,5,0,1,0),
  IU   = c(2,0,0,0,0,0,2,0,0,0),
  CIU  = c(0,0,0,0,0,0,6,2,1,1),
  ERC  = c(0,0,0,0,0,0,4,2,1,1),
  BNG  = c(0,1,1,0,0,0,0,0,0,0),
  UPYD = c(0,0,0,0,0,0,0,0,0,0),
  id_elec = "02-2004-03-14"
)

seats_2008 <- tibble(
  prov = c(
    "Madrid","Pontevedra","La Coruña/A Coruña","Lugo","Orense/Ourense",
    "Murcia","Barcelona","Girona","Tarragona","Lleida"
  ),

  PSOE = c(15,3,3,2,2,3,16,3,4,2),
  PP   = c(18,3,4,2,2,7,6,0,1,1),
  IU   = c(1,0,0,0,0,0,1,0,0,0),
  CIU  = c(0,0,0,0,0,0,6,2,1,1),
  ERC  = c(0,0,0,0,0,0,2,1,0,0),
  BNG  = c(0,1,1,0,0,0,0,0,0,0),
  UPYD = c(1,0,0,0,0,0,0,0,0,0),
  id_elec = "02-2008-03-09"
)

seats_2011 <- tibble(
  prov = c("Madrid", "Pontevedra", "La Coruña/A Coruña", "Lugo", "Orense/Ourense",
           "Murcia", "Barcelona", "Girona", "Tarragona", "Lleida"),
  PP   = c(19, 4, 5, 3, 3, 8, 7, 1, 2, 1),
  PSOE = c(10, 2, 2, 1, 1, 2,10, 1, 2, 1),
  UPYD = c( 4, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  IU   = c( 3, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  CIU  = c( 0, 0, 0, 0, 0, 0, 9, 3, 2, 2),
  ERC  = c( 0, 0, 0, 0, 0, 0, 2, 1, 0, 0),
  ICV  = c( 0, 0, 0, 0, 0, 0, 3, 0, 0, 0),
  BNG  = c( 0, 1, 1, 0, 0, 0, 0, 0, 0, 1),
  id_elec = "02-2011-11-20"
)

seats_2015 <- tibble(
  prov = c(
    "Madrid","Pontevedra","La Coruña/A Coruña","Lugo","Orense/Ourense",
    "Murcia","Barcelona","Girona","Tarragona","Lleida"
  ),

  PP      = c(13,3,3,2,2,5,4,0,1,0),
  PSOE    = c( 6,2,2,1,1,2,5,1,1,1),
  PODEMOS = c( 8,0,0,0,0,1,0,0,0,0),
  ECP     = c( 0,0,0,0,0,0,9,1,1,1),
  CS      = c( 7,0,1,0,0,2,4,0,1,0),
  DIL     = c( 0,0,0,0,0,0,4,2,1,1),
  ERC     = c( 0,0,0,0,0,0,5,2,1,1),
  IU      = c( 2,0,0,0,0,0,0,0,0,0),
  ENMAREA = c( 0,2,2,1,1,0,0,0,0,0),

  id_elec = "02-2015-12-20"
)

seats_2016 <- tibble(
  prov = c(
    "Madrid","Pontevedra","La Coruña/A Coruña","Lugo","Orense/Ourense",
    "Murcia","Barcelona","Girona","Tarragona","Lleida"
  ),
  PP               = c(15,3,4,2,3,5,4,0,1,1),
  PSOE             = c( 7,2,2,1,1,2,5,1,1,0),
  PODEMOS          = c( 8,0,0,0,0,1,0,0,0,0),
  ECP              = c( 0,0,0,0,0,0,9,1,1,1),
  CS               = c( 6,0,0,0,0,2,4,0,1,0),
  CDC              = c( 0,0,0,0,0,0,4,2,1,1),
  `ERC-CATSI`      = c( 0,0,0,0,0,0,5,2,1,1),
  ENMAREA          = c( 0,2,2,1,0,0,0,0,0,0),

  id_elec = "02-2016-06-26"
)

seats_2019a <- tibble(
  prov = c(
    "Madrid", "Pontevedra", "La Coruña/A Coruña", "Lugo", "Orense/Ourense",
    "Murcia", "Barcelona", "Girona", "Tarragona", "Lleida"
  ),
  PSOE           = c(11, 3, 3, 2, 2, 3, 9, 1, 1, 1),
  PP             = c(7, 2, 3, 2, 2, 2, 1, 0, 0, 0),
  CS             = c(8, 1, 1, 0, 0, 2, 4, 0, 1, 0),
  PODEMOS        = c(6, 1, 1, 0, 0, 1, 0, 0, 0, 0),
  VOX            = c(5, 0, 0, 0, 0, 2, 1, 0, 0, 0),
  MP             = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  ECP            = c(0, 0, 0, 0, 0, 0, 6, 0, 1, 0),
  ERC            = c(0, 0, 0, 0, 0, 0, 8, 3, 2, 2),
  `JXCAT-JUNTS`  = c(0, 0, 0, 0, 0, 0, 3, 2, 1, 1),
  id_elec = "02-2019-04-28"
)

seats_2019n <- tibble(
  prov = c(
    "Madrid", "Pontevedra", "La Coruña/A Coruña", "Lugo", "Orense/Ourense",
    "Murcia", "Barcelona", "Girona", "Tarragona", "Lleida"
  ),
  PSOE          = c(10, 3, 3, 2, 2, 3, 8, 1, 2, 1),
  PP            = c(10, 3, 3, 2, 2, 3, 2, 0, 0, 0),
  CS            = c(3, 0, 0, 0, 0, 0, 2, 0, 0, 0),
  PODEMOS       = c(5, 1, 1, 0, 0, 1, 0, 1, 1, 0),
  VOX           = c(7, 0, 0, 0, 0, 3, 2, 0, 0, 0),
  MP            = c(2, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  ECP           = c(0, 0, 0, 0, 0, 0, 5, 1, 1, 0),
  ERC           = c(0, 0, 0, 0, 0, 0, 7, 2, 2, 2),
  `JXCAT-JUNTS` = c(0, 0, 0, 0, 0, 0, 4, 2, 1, 1),
  CUP           = c(0, 0, 0, 0, 0, 0, 2, 0, 0, 0),
  BNG           = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  id_elec = "02-2019-11-10"
)

seats_2023 <- tibble(
  prov = c(
    "Madrid","Pontevedra","La Coruña/A Coruña","Lugo","Orense/Ourense",
    "Murcia","Barcelona","Girona","Tarragona","Lleida"
  ),
  PSOE          = c(10,3,2,1,1,3,13,2,2,2),
  PP            = c(16,3,4,3,3,4, 5,0,1,0),
  SUMAR         = c( 6,1,1,0,0,1, 5,1,1,0),
  VOX           = c( 5,0,0,0,0,2, 2,0,0,0),
  ERC           = c( 0,0,0,0,0,0, 4,1,1,1),
  `JXCAT-JUNTS` = c( 0,0,0,0,0,0, 3,2,1,1),
  BNG           = c( 0,0,1,0,0,0, 0,0,0,0),

  id_elec = "02-2023-07-24"
)

seats_by_province <-
  bind_rows(seats_1982, seats_1986, seats_1989, seats_1993,
            seats_1996, seats_2000, seats_2004, seats_2008,
            seats_2011, seats_2015, seats_2016, seats_2019a,
            seats_2019n, seats_2023) |>
  left_join(pollspain::cod_INE_prov_ccaa, by = "prov") |>
  mutate("id_INE_prov" = paste0(cod_INE_ccaa, "-", cod_INE_prov))

id_cols <- c("prov", "id_elec",
             "cod_INE_ccaa", "cod_INE_prov", "ccaa", "cod_MIR_ccaa", "id_INE_prov")

seats_by_province <- seats_by_province |>
  pivot_longer(
    cols = -any_of(id_cols),
    names_to  = "abbrev_candidacies",
    values_to = "real_seats",
    values_drop_na = TRUE
  ) |>
  mutate(real_seats = as.integer(real_seats)) |>
  arrange(id_elec, prov, desc(real_seats), abbrev_candidacies) |>
  filter(real_seats > 0)

candidacies_id <- global_dict_parties |>
  select(abbrev_candidacies, id_candidacies, id_candidacies_nat, id_elec) |>
  distinct(id_elec, abbrev_candidacies, .keep_all = TRUE)

seats_by_province <- seats_by_province |>
  left_join(candidacies_id, by = c("id_elec", "abbrev_candidacies"))

seats_by_province <-
  seats_by_province |>
  mutate(across(where(is.character), \(x) enc2utf8(x)))

usethis::use_data(seats_by_province, overwrite = TRUE,
                  compress = "xz")
