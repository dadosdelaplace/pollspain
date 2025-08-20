# ----- required libraries -----

library(dplyr)
library(tidyr)
library(purrr)
library(pollspain)
library(stringr)
library(stringi)

# ----- congress elections -----

raw_data_2023 <- import_candidacies_data("congress", year = 2023, short_version = FALSE)
raw_data_2016 <- import_candidacies_data("congress", year = 2016, short_version = FALSE)
raw_data_2019_11 <- import_candidacies_data("congress", date = "2019-11-10", short_version = FALSE)
raw_data_2019_04 <- import_candidacies_data("congress", date = "2019-04-28", short_version = FALSE)
raw_data_2015 <- import_candidacies_data("congress", year = 2015, short_version = FALSE)
raw_data_2011 <- import_candidacies_data("congress", year = 2011, short_version = FALSE)
raw_data_2008 <- import_candidacies_data("congress", year = 2008, short_version = FALSE)
raw_data_2004 <- import_candidacies_data("congress", year = 2004, short_version = FALSE)
raw_data_2000 <- import_candidacies_data("congress", year = 2000, short_version = FALSE)
raw_data_1996 <- import_candidacies_data("congress", year = 1996, short_version = FALSE)
raw_data_1993 <- import_candidacies_data("congress", year = 1993, short_version = FALSE)
raw_data_1989 <- import_candidacies_data("congress", year = 1989, short_version = FALSE)
raw_data_1986 <- import_candidacies_data("congress", year = 1986, short_version = FALSE)
raw_data_1982 <- import_candidacies_data("congress", year = 1982, short_version = FALSE)

# in a list due to memory issues
data_list <-
  list(raw_data_2023, raw_data_2019_11, raw_data_2019_04,
       raw_data_2016, raw_data_2015,
       raw_data_2011, raw_data_2008, raw_data_2004, raw_data_2000,
       raw_data_1996, raw_data_1993, raw_data_1989, raw_data_1986,
       raw_data_1982)
rm(list =
     c("raw_data_2023", "raw_data_2016", "raw_data_2019_11", "raw_data_2019_04",
       "raw_data_2015", "raw_data_2011", "raw_data_2008",
       "raw_data_2004", "raw_data_2000", "raw_data_1996",
       "raw_data_1993", "raw_data_1989", "raw_data_1986",
       "raw_data_1982"))

# ----- first preproc -----
preproc_abbrev <- function(data) {

  first_abbrev <-
    data |>
    group_by(id_candidacies_nat) |> # group by national candidacy code
    slice(1) |> # in case of ties
    select(id_candidacies_nat, abbrev_candidacies)

  dict <-
    data |>
    left_join(first_abbrev, by = "id_candidacies_nat",
              suffix = c("", ".rm")) |>
    select(id_elec, abbrev_candidacies, name_candidacies,
           id_candidacies_nat, id_candidacies)  |>
    distinct(id_candidacies, .keep_all = TRUE)

  return(dict)

}

raw_global_dict_parties <-
  map_dfr(data_list, preproc_abbrev) |>
  # otherwise, Nación Andaluza --> NA as abbrev
  mutate("abbrev_candidacies" =
           if_else(str_detect(str_to_upper(name_candidacies),
                              "NACIÓN ANDALUZA|NACION ANDALUZA"),
                   "NAND", abbrev_candidacies)) |>
  drop_na() |>
  arrange(abbrev_candidacies)
rm(data_list)

# ----- refine preproc -----

global_dict_parties <-
  raw_global_dict_parties |>
  mutate("name_candidacies" = str_squish(str_to_upper(name_candidacies)),
         "name_candidacies" = str_replace_all(name_candidacies, " -  ", " - "),
         "name_candidacies" = str_replace_all(name_candidacies, "- | -", " - "),
         "name_candidacies" = str_replace_all(name_candidacies, "\\+", ""),
         "name_candidacies" = stri_trans_general(name_candidacies, "Latin-ASCII"),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "ANDECHA"), "ANDECHA", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "ADELANTE ANDALUCIA"), "AA", abbrev_candidacies),
         "abbrev_candidacies" = str_replace_all(abbrev_candidacies, "\\+", ""),
         "abbrev_candidacies" = stri_trans_general(abbrev_candidacies, "Latin-ASCII"),
         "abbrev_candidacies" = str_remove_all(str_squish(str_to_upper(abbrev_candidacies)), " "),
         "abbrev_candidacies" = str_remove_all(abbrev_candidacies, "\\.|\\,"),
         "abbrev_candidacies" = str_remove_all(abbrev_candidacies, "\\'|\\`|\\´"),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "ESQUERRA", "ERC", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "IU-CA", "IU", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PCA-PCE", "PCE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PCPA", "PCPE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PCPC", "PCPE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "PSOE"), "PSOE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PA", "PSA-PA", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "EE", "PSE-EE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "RUIZ-MATEOS" |
                     str_detect(name_candidacies, "RUIZ-MATEOS"),
                   "ARM", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "CHUNTA ARAGONESISTA"),
                   "CHA", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "FE (I)|FE(I)", "FE-INDEP", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PNV-NV" |
                     abbrev_candidacies == "PNV-EAJ" |
                     abbrev_candidacies == "EAJ-PNV", "PNV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "AP-PDP|AP-PL"), "AP-PDP-PL", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "LV-LV" |
                     abbrev_candidacies == "PEE-(LV)",
                   "LV", abbrev_candidacies),
         "name_candidacies" = str_replace_all(name_candidacies, "ALIANZA POP\\.", "ALIANZA POPULAR "),
         "name_candidacies" = str_replace_all(name_candidacies, "P\\.COMUNISTA", "PARTIDO COMUNISTA"),
         "name_candidacies" = str_replace_all(name_candidacies, "BLOQUE PSG", "BLOQUE NACIONALISTA GALEGO"),
         "abbrev_candidacies" = str_replace_all(abbrev_candidacies, "B-PSG", "BNG"),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "CONSERVADORS DE CATALUNYA"), "CIC", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "PARTIDO PROVERISTA"),
                   "PPROV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "PARTIDO SOCIALISTA DE ARAGON"),
                   "PSARAG", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == '\"JF\"', "JF", abbrev_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "PSE-EE", "PARTIDO SOCIALISTA DE EUSKADI - EUSKADIKO EZKERRA",
                   name_candidacies),
         "name_candidacies" =
           if_else(name_candidacies == "CENTRO DEMOCRATICO Y SOCIAL\\.",
                   "CENTRO DEMOCRATICO Y SOCIAL", name_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PCE (M-L)|PCE(M-L)" |
                     name_candidacies == "PARTIDO COMUNISTA DE ESPAÑA (MARXISTA-LENINISTA)" |
                     name_candidacies == "PARTIT COMUNISTA D'ESPANYA (MARXISTA-LENINISTA)",
                   "PCE-M-L", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "EA-EUE") |
                     str_detect(name_candidacies, "EUSKO ALKARTASUNA"),
                   "EA-EUE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "ICV") |
                     str_detect(name_candidacies, "INICIATIVA PER CATALUNYA"),
                   "ICV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "NA-SUMA") |
                     str_detect(name_candidacies, "NAVARRA SUMA"),
                   "NA-SUMA", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "LIT-CI|LITCI") |
                     str_detect(name_candidacies, "LIGA INTERNACIONAL DE TRABAJADORES"),
                   "LIT-CI", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "BNG|BNG-NOS") |
                     str_detect(name_candidacies, "BLOQUE NACIONALISTA GALEGO"),
                   "BNG", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "LV-LV|LOS VERDES|VERDES") |
                     str_detect(name_candidacies, "LOS VERDES"),
                   "LV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "DL|DIL") |
                     str_detect(name_candidacies, "DEMOCRACIA Y LIBERTAD"),
                   "DIL", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "IZQUIERDA UNIDA|ESQUERRA UNIDA|EZKER ANITZA|UNITAT POPULAR|UNIDAD POPULAR|ESQUERDA") &
                     str_detect(abbrev_candidacies, "IU|UP-UPEC|IU-UPEC|UPEC-IU|UNIDADPOPU|LV|UPEC|^EU|-EU"),
                   "IU", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else((str_detect(abbrev_candidacies, "COMPROMIS") |
                     str_detect(name_candidacies, "COMPROMIS")) &
                     !str_detect(abbrev_candidacies, "IU"),
                   "COMPROMIS", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "CUP-PR") |
                     str_detect(name_candidacies, "CANDIDATURA UNITAT POPULAR"),
                   "CUP", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "EHBILDU|EH BILDU") |
                     str_detect(name_candidacies, "EHBILDU|EH BILDU"),
                   "EH-BILDU", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "TERUEL EXISTE | TE") |
                     str_detect(name_candidacies, "TERUEL EXISTE"),
                   "TE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "3E", "T3EA", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "AHORA CANARIAS"),
                   "AHORA-CANARIAS", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "ANTICAPITALIST"),
                   "ANTICAPITALISTAS", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "APROME NACIONALISTA"),
                   "APROME", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "ARA, PAIS VALENCIA"),
                   "ARA-PV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "ARALAR"),
                   "ARALAR", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "ESPANA VACIADA"),
                   "EV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "BLOC"),
                   "BLOC-EV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "CCA"),
                   "CC", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "CDS") &
                     str_detect(name_candidacies, "CENTRO DEMOCRATICO"),
                   "CDS", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "COALICION SOCIAL DEMOCRATA Y LIBERAL") |
                     str_detect(name_candidacies, "COALICION SOCIALDEMOCRATA Y LIBERAL"),
                   "CSD-L", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "ESCANOS EN BLANCO|ESCONS EN BLANC"),
                   "EB", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "EZKER BATUA"),
                   "IU", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "SUMAR"),
                   "SUMAR", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "MAREA|PODEMOS-EN"),
                   "ENMAREA", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "PODEMOS-EU$|PODEMOS-EU-$"),
                   "EN COMUN", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "PODEMOS"),
                   "PODEMOS", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies %in%
                     c("PP-FORO", "PP-PAR", "PP-UP", "PP-EU", "PP-CDEG"),
                   "PP", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies %in% c("PSARAG", "PSC") |
                     str_detect(abbrev_candidacies, "PSE-EE"),
                   "PSOE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "SALUD Y ECOLOGIA EN SOLIDARIDAD",
                   "SALUDECO", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "RECORTES CERO"),
                   "RECORTES-CERO", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PSA",
                   "PSA-PA", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "PCE"),
                   "PCE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "PCPE"),
                   "PCPE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "PDCAT"),
                   "PDCAT", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "ECP-GUANYEM") |
                     str_detect(abbrev_candidacies, "ENCOMU"),
                   "ECP", abbrev_candidacies),
         "name_candidacies" =
           str_replace(name_candidacies, '\"EN COMU PODEM\"', "EN COMU PODEM"),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "ESQUERRA NACIONALISTA VALENCIANA"),
                   "ENV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "PARTIT ECOLOGISTA DE CATALUNYA"),
                   "PEC", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "ESQUERRA REPUBLICANA DE CATALUNYA"),
                   "ERC", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "ES2000|ESPANA2000") |
                     str_detect(name_candidacies, "PLATAFORMA ESPANA 2000"),
                   "E-2000", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "ESPANA VACIADA"),
                   "EV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(abbrev_candidacies, "ESQUERRA-PV"),
                   "ERPV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "ELS VERDS"),
                   "LV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "FE(A)", "FE-A", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies %in% c("FE(I)", "FEI") |
                     str_detect(name_candidacies, "FALANGE ESPANOLA INDEPENDIENTE"),
                   "FE-I", abbrev_candidacies),
         "name_candidacies" =
           if_else(str_detect(name_candidacies, "FALANGE ESPANOLA DE LAS J.O.N.S."),
                   "FALANGE ESPANOLA DE LAS JONS", name_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "FALANGE ESPANOLA DE LAS JONS"),
                   "FE-JONS", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "MAS PAIS"),
                   "MP", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "UC-CDS",
                   "CDS", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "CC-PNC-PIL",
                   "CC-PNC", abbrev_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "AIC",
                   "AGRUPACIONES INDEPENDIENTES DE CANARIAS", name_candidacies),
         "name_candidacies" =
           if_else(name_candidacies == "UNION REGIONALISTA DE CASTILLA Y LEON",
                   "UNIDAD REGIONALISTA DE CASTILLA Y LEON", name_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "UNION DEMOCRATICA ALICANTINA",
                   "UDEMALI", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "UNIO MALLORQUINA"),
                   "UM", abbrev_candidacies),
         "name_candidacies" =
           if_else(str_detect(name_candidacies, "UNIO MALLORQUINA"),
                   "UNIO MALLORQUINA", name_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "SOM VALENCIANS EN MOVIMENT"),
                   "UIG-SOM-CUI", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "UNION CASTELLANISTA"),
                   "UCAST", abbrev_candidacies),
         "name_candidacies" =
           if_else(str_detect(name_candidacies, "UNION PROGRESO Y DEMOCRACIA") |
                     str_detect(name_candidacies, "UNION, PROGRESO Y DEMOCRACIA"),
                   "UNION PROGRESO Y DEMOCRACIA", name_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "UPN-PP",
                   "UNION DEL PUEBLO NAVARRO-PARTIDO POPULAR", name_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "UPN-PP",
                   "PP", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "UPEC-IU", "IU", abbrev_candidacies),
         "name_candidacies" =
           if_else(str_detect(name_candidacies, "TERUEL EXISTE"),
                   "TERUEL EXISTE", name_candidacies),
         "name_candidacies" =
           if_else(str_detect(name_candidacies, "SOLIDARIDAD Y AUTOGESTION INTERNACIONALISTA"),
                   "PARTIDO SOLIDARIDAD Y AUTOGESTION INTERNACIONALISTA", name_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "RISA",
                   "PARTIDO REPUBLICANO INDEPENDIENTE SOLIDARIO ANDALUZ",
                   name_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PSM-EN", "PSM-ENE", abbrev_candidacies),
         "name_candidacies" =
           if_else(str_detect(name_candidacies, "PARTIDO NACIONALISTA VASCO"),
                   "EAJ - PARTIDO NACIONALISTA VASCO",
                   name_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "PARTIDO NACIONALISTA CANARIO", "PNCANAR", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "PARTIDO CANTONALISTA", "PCANTONAL", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "PARTIDO NACIONALISTA CANTABRO", "PNCANTAB", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "PARTIDO NACIONALISTA CEUTI", "PNCEUTI", abbrev_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "PLCI",
                   "PARTIDO DE LAS LIBERTADES CIVILES",
                   name_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PFIV", "PFYV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PHA", "PH", abbrev_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "PFYV",
                   "PARTIDO FAMILIA Y VIDA", name_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "VERTICE ESPANOL"),
                   "VERTICE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PEC-VERDE", "PEC", abbrev_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "PCTR",
                   "PARTIDO CENTRISTA", name_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "PCTE/ELAK", "PCTE", abbrev_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "PCPG",
                   "PARTIDO COMUNISTA DO POVO GALEGO", name_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "PARTIDO CANTONALISTA DE CASTELLON",
                   "PCANTOC", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "PARTIDO AGRARIO ESPANOL",
                   "PAGRAESP", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "MOVIMIENTO COMUNISTA DE EXTREMADURA",
                   "MCEX", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "MOVIMIENTO CATOLICO ESPANOL",
                   "MCE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "MOVIMIENTO ARAGONES SOCIAL",
                   "MARSO", abbrev_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "LN",
                   "LIBERTAD NAVARRA", name_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "LKI",
                   "LIGA KOMUNISTA IRAULTZAILEA", name_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "JXCAT-JUNTS",
                   "JUNTS PER CATALUNYA", name_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "IRV",
                   "IDENTIDAD REINO DE VALENCIA", name_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "ELPI",
                   "EL PI - PROPOSTA PER LES ILLES BALEARS", name_candidacies),
         "name_candidacies" =
           if_else(str_detect(name_candidacies, "CENTRO DEMOCRATICO SOCIAL"),
                   "CENTRO DEMOCRATICO Y SOCIAL", name_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "ECP",
                   "EN COMU PODEM", name_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "EH-BILDU",
                   "EUSKAL HERRIA BILDU", name_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "CDN",
                   "CONVERGENCIA DE DEMOCRATAS DE NAVARRA", name_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "BIA",
                   "BLOQUE DE LA IZQUIERDA ASTURIANA", name_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "INIC.CIUDADANA VASCA/EUSKAL HURITARREN EGITEKOAK",
                   "INICV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "P.SOCIALDEMOCRATA IND. DE LA COMUNITAT VALENCIANA",
                   "PSICV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "FRENTE ESPANOL",
                   "FESP", abbrev_candidacies),
         "name_candidacies" =
           if_else(str_detect(name_candidacies, "ESQUERRA UNIDA DEL PAIS VALENCIA-IZQUIERDA REPUBLICANA"),
                   "ESQUERRA UNIDA DEL PAIS VALENCIA-IZQUIERDA REPUBLICANA", name_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "ESQUERRA REPUBLICANA DEL PAIS VALENCIA"),
                   "ERPV", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "FALANGE ASTURIANA",
                   "FASTUR", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "ELAK/PCTE",
                   "ELAK-PCTE", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "EXTREMADURA UNIDA",
                   "EXTUNI", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "CENTRO DEMOCRATICO LIBERAL",
                   "CDL", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "PARTIDO ASTURIANO DEMOCRATA LIBERAL",
                   "PASTURDL", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "PARTIDO DE LOS PENSIONISTAS EN ACCION",
                   "PPENSION", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "UNION DE AUTONOMIAS",
                   "UAUTONOM", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "CENTRISTES DE CATALUNYA",
                   "UCD", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "COMPROMISO POR GALICIA",
                   "CPGAL", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(abbrev_candidacies == "ACI" |
                     str_detect(name_candidacies, "AGRUPACION CIUDADANA INDEPENDIENTE"),
                   "ACI", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(str_detect(name_candidacies, "AHORA-CANARIAS"),
                   "AHORA-CANARIAS", abbrev_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies %in% c("COALICION CANARIA-NUEVA CANARIAS",
                                           "NUEVA CANARIAS-COALICION CANARIA"),
                   "CC-NC", abbrev_candidacies),
         "name_candidacies" =
           if_else(abbrev_candidacies == "AMD",
                   "ALTERNATIVA MOTOR Y DEPORTE", name_candidacies),
         "abbrev_candidacies" =
           if_else(name_candidacies == "COALICION EXTREMENA",
                   "COALIEXT", abbrev_candidacies),
         "name_candidacies" =
           if_else(name_candidacies == "ALIANZA NACIONAL",
                   "ALIANZA POR LA UNIDAD NACIONAL", name_candidacies),
         "name_candidacies" =
           if_else(str_detect(name_candidacies, "ESQUERRA NACIONALISTA VALENCIANA"),
                   "ESQUERRA NACIONALISTA VALENCIANA", name_candidacies),
         "name_candidacies" =
           if_else(str_detect(name_candidacies, "PARTIDO ANIMALISTA CONTRA EL MALTRATO ANIMAL") |
                     name_candidacies == "PARTIDO ANIMALISTA-CONTRA EL MALTRATO ANIMAL",
                   "PARTIDO ANIMALISTA CONTRA EL MALTRATO ANIMAL", name_candidacies)) |>
  mutate("name_candidacies_nat" = name_candidacies,
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "SUMAR", "SUMAR", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "AHORA-CANARIAS", "AHORA-CANARIAS", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PSOE", "PARTIDO SOCIALISTA OBRERO ESPANOL", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PP", "PARTIDO POPULAR", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PUMJ", "POR UN MUNDO MAS JUSTO", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "RECORTES-CERO", "RECORTES-CERO", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PSM-ENE", "PSM ENTESA NACIONALISTA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PSA-PA",
                   "PARTIDO SOCIALISTA DE ANDALUCIA - PARTIDO ANDALUZ", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PREPAL",
                   "PARTIDO REGIONALISTA DEL PAIS LEONES", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PP-UPM",
                   "PARTIDO POPULAR", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "ACI",
                   "ACI PANTERAS GRISES DE ESPANA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CCD",
                   "COALICION DE CENTRO DEMOCRATICO", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CHA",
                   "CHUNTA ARAGONESISTA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "POSI",
                   "PARTIDO OBRERO SOCIALISTA INTERNACIONALISTA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PNV",
                   "EAJ - PARTIDO NACIONALISTA VASCO", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PORE",
                   "PARTIDO DE LOS OBREROS REVOLUCIONARIOS DE ESPANA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "POR",
                   "PARTIDO OBRERO REVOLUCIONARIO", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PODEMOS",
                   "PODEMOS", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PLN",
                   "PARTIDO DE LA LEY NATURAL", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "BLOC-EV",
                   "BLOC NACIONALISTA VALENCIA - ESQUERRA VERDA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PICC",
                   "PLATAFORMA INDEPENDIENTE CIUDADANA DE CATALUNA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PH",
                   "PARTIDO HUMANISTA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PED",
                   "PARTIDO ESPANOL DEMOCRATICO", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PRT",
                   "PARTIDO REVOLUCIONARIO DE LOS TRABAJADORES", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "VERTICE",
                   "VERTICE ESPANOL", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PST",
                   "PARTIDO SOCIALISTA DE LOS TRABAJADORES", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PEC",
                   "PARTIT ECOLOGISTA DE CATALUNYA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies %in% c("PCTE", "ELAK-PCTE", "PCTE-ELAK"),
                   "PARTIDO COMUNISTA DE LOS TRABAJADORES DE ESPANA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PCPE",
                   "PARTIDO COMUNISTA DE LOS PUEBLOS DE ESPANA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PCE",
                   "PARTIDO COMUNISTA DE ESPANA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "ARDE",
                   "ACCION REPUBLICANA DEMOCRATICA ESPANOLA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "ARALAR",
                   "ARALAR", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "AP-PDP-PL",
                   "ALIANZA POPULAR", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "ANTICAPITALISTAS",
                   "ANTICAPITALISTAS", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "AR",
                   "ACCION REPUBLICANA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "ARM",
                   "AGRUPACION RUIZ-MATEOS", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PAR",
                   "PARTIDO ARAGONES", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CS",
                   "CIUDADANOS PARTIDO DE LA CIUDADANIA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CR",
                   "COALICION REPUBLICANA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CC",
                   "COALICION CANARIA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CC-NC",
                   "COALICION CANARIA - NUEVA CANARIAS", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CC-NC-PNC",
                   "COALICION CANARIA - NUEVA CANARIAS - PARTIDO NACIONALISTA DE CANARIAS",
                   name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CC-PNC",
                   "COALICION CANARIA - PARTIDO NACIONALISTA DE CANARIAS",
                   name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "COMPROMIS",
                   "COMPROMIS", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CA",
                   "COALICION ANDALUCISTA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "BNG",
                   "BLOQUE NACIONALISTA GALEGO", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CSD",
                   "COALICION SOCIALDEMOCRATA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CSD-L",
                   "COALICION SOCIALDEMOCRATA Y LIBERAL", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PAE",
                   "PARTIDO DE LOS TRABAJADORES AUTONOMOS DE ESPANA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "PACMA",
                   "PARTIDO ANIMALISTA CON EL MEDIO AMBIENTE", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "P-LIB",
                   "PARTIDO LIBERTARIO", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "OV",
                   "OS VERDES DE GALICIA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "CDS",
                   "CENTRO DEMOCRATICO Y SOCIAL", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "NOS",
                   "COALICION POR UN NUEVO PARTIDO SOCIALISTA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "MSR",
                   "MOVIMIENTO SOCIAL REPUBLICANO", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "MP",
                   "MAS PAIS", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "LIT-CI",
                   "LUCHA INTERNACIONALISTA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "LG",
                   "PARTIDO DE LA GENTE", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "IU",
                   "IZQUIERDA UNIDA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "ICV",
                   "INICIATIVA PER CATALUNYA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "HB",
                   "HERRI BATASUNA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "EB",
                   "ESCANOS EN BLANCO", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "E-2000",
                   "ESPANA 2000", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "EA-EUE",
                   "EUSKO ALKARTASUNA - EUSKAL EZKERRA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "IR-PRE",
                   "PARTIDO IZQUIERDA REPUBLICANA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "FN",
                   "FUERZA NUEVA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "FE-I",
                   "FALANGE ESPANOLA INDEPENDIENTE", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "LV",
                   "LOS VERDES", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "EU",
                   "ESQUERRA UNIDA DE LES ILLES BALEARS", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "NPS",
                   "COALICION POR UN NUEVO PARTIDO SOCIALISTA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "ERPV",
                   "ESQUERRA REPUBLICANA DEL PAIS VALENCIA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies %in% c("EKA", "EKA-PC"),
                   "PARTIDO CARLISTA", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies %in% c("EI", "EI-ADD"),
                   "ESCONS INSUBMISOS", name_candidacies_nat),
         "name_candidacies_nat" =
           if_else(abbrev_candidacies == "ERC",
                   "ESQUERRA REPUBLICANA DE CATALUNYA", name_candidacies_nat)) |>
  mutate(name_candidacies = str_squish(name_candidacies),
         name_candidacies_nat = str_squish(name_candidacies_nat))

# global_dict_parties |>
#   distinct(abbrev_candidacies, name_candidacies) |>
#   arrange(abbrev_candidacies)

# colors
global_dict_parties <-
  global_dict_parties |>
  mutate(color =
           case_when(abbrev_candidacies == "UCD" ~ "#ff7f27",
                     abbrev_candidacies %in% c("AP-PDP-PL", "PP","PP-UPM") |
                       str_detect(name_candidacies, "PARTIDO POPULAR") ~ "#1e4b8f",
                     abbrev_candidacies == "PSOE" ~ "#e30613",
                     abbrev_candidacies == "PCE" ~ "#be1622",
                     abbrev_candidacies %in% c("CIU", "PDECAT-E-CIU") ~ "#18307b",
                     abbrev_candidacies == "JXCAT-JUNTS" ~ "#00c3b2",
                     abbrev_candidacies == "PNV" ~ "#2a8343",
                     abbrev_candidacies == "PSA-PA" ~ "#9cd4a4",
                     abbrev_candidacies == "HB" ~ "#633000",
                     abbrev_candidacies %in% c("ERC", "ERC-CATSI") ~ "#f3c54b",
                     abbrev_candidacies == "CDS" ~ "#b1c444",
                     abbrev_candidacies == "IU" ~ "#a9272f",
                     abbrev_candidacies == "EA-EUE" ~ "#69ab60",
                     abbrev_candidacies %in% c("CC", "CC-NC-PNC", "CC-PNC", "NC-CCN", "CC-NC") ~ "#fbd44c",
                     abbrev_candidacies == "BNG" ~ "#8dd6ff",
                     abbrev_candidacies %in% c("NA-BAI", "GBAI") ~ "#e68271",
                     abbrev_candidacies == "UPYD" ~ "#da1d79",
                     abbrev_candidacies == "CHA" ~ "#3c3c3c",
                     abbrev_candidacies == "AMAIUR" ~ "#0198b3",
                     abbrev_candidacies == "COMPROMIS" ~ "#e07726",
                     abbrev_candidacies == "PODEMOS" ~ "#8f87d4",
                     abbrev_candidacies == "CS" ~ "#ff6a2d",
                     abbrev_candidacies == "ECP" ~ "#ec4423",
                     abbrev_candidacies == "ENMAREA" ~ "#1450ff",
                     abbrev_candidacies == "EH-BILDU" ~ "#00d0b6",
                     abbrev_candidacies == "CDC" ~ "#00447b",
                     abbrev_candidacies == "VOX" ~ "#5ac035",
                     abbrev_candidacies == "NA-SUMA" ~ "#ae191d",
                     abbrev_candidacies == "PRC" ~ "#b7bf10",
                     abbrev_candidacies == "MP" ~ "#0fddc4",
                     abbrev_candidacies == "CUP" ~ "#ffdd4c",
                     abbrev_candidacies == "SUMAR" ~ "#ec4771",
                     abbrev_candidacies == "UPN" ~ "#2a52be",
                     TRUE ~ NA_character_))

global_dict_parties <- global_dict_parties |>
  mutate(abbrev_candidacies_unified = case_when(
    str_detect(abbrev_candidacies, "^PP$|^PP-") ~ "PP",
    str_detect(abbrev_candidacies, "^CC$|CC-NC|CC-PNC") ~ "CC",
    str_detect(abbrev_candidacies, "CCD") ~ "CCD",
    str_detect(abbrev_candidacies, "CCS") ~ "CCS",
    str_detect(abbrev_candidacies, "CSD") ~ "CSD",
    str_detect(abbrev_candidacies, "^EI|EI-ADD") ~ "EI",
    str_detect(abbrev_candidacies, "EKA|PARTIDOCARLISTA|^PC$|^PCA$|^PCAC") ~ "PARTIDOCARLISTA",
    str_detect(abbrev_candidacies, "CNB|CENB") ~ "CENB",
    str_detect(abbrev_candidacies, "^ERC$|ERC-CATSI") ~ "ERC",
    str_detect(name_candidacies_nat, "LA FALANGE|FALANGE ESPA") ~ "FE",
    str_detect(name_candidacies_nat, "PODEMOS") ~ "PODEMOS",
    str_detect(name_candidacies_nat, "PARTIDO SOCIAL DEMOCRATA") ~ "PSD",
    TRUE ~ abbrev_candidacies
  )) |>
  arrange(abbrev_candidacies_unified)

global_dict_parties <-
  global_dict_parties |>
  mutate(across(where(is.character), \(x) enc2utf8(x)))

global_dict_parties<- global_dict_parties |>
  mutate(id_candidacies_unified = as.integer(factor(abbrev_candidacies_unified)))

usethis::use_data(global_dict_parties, overwrite = TRUE,
                  compress = "xz")
