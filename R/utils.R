
#' @title Conversion type election to code
#'
#' @description Conversion the type of election (referendum,
#' congress, etc) to a properly code (according to documentation)
#' available in Spanish Ministry of Interior
#'
#' @param type_elec Type elections for which data is available.
#' It should be one of the following values: "referendum",
#' "congress", "senate", "local", "cabildo" (Canarian council)
#' or "EU".
#'
#' @return A vector of string codes (\code{cod_elec}) as follows:
#' \itemize{
#'   \item \code{"01"}: referendum.
#'   \item \code{"02"}: congress.
#'   \item \code{"03"}: senate.
#'   \item \code{"04"}: local elections.
#'   \item \code{"06"}: cabildo - Canarian council - elections).
#'   \item \code{"07"}: European Parlament elections.
#' }
#'
#' @author Javier Alvarez-Liebana, David Pereiro-Pol, Mafalda
#' Gonzalez-Gonzalez and Irene Bosque-Gala.
#' @source Data extracted from
#' \url{https://infoelectoral.interior.gob.es/opencms/es/elecciones-celebradas/area-de-descargas/}{Spanish Ministry of Interior}
#' @keywords utils
#' @name type_to_code_election
#'
#' @examples
#' ## Correct examples
#'
#' type_to_code_election(type_elec = "congress")
#' type_to_code_election(type_elec = "senate")
#' type_to_code_election(type_elec = "local")
#'
#' # ----
#' # Incorrect examples
#' # ----
#'
#' \dontrun{
#' type_to_code_election(type_elec = "hi!")
#' type_to_code_election(type_elec = "nop")
#' type_to_code_election(type_elec = "national")
#' }
#'
#' @export
type_to_code_election <- function(type_elec) {

  # Check: if value in type_elec is allowed
  if (!all(type_elec %in%
        c("referendum", "congress", "senate", "local", "regional",
          "cabildo", "EU"))) {

    stop("Input argument type_elec is not valid: it must be taken one of the following values: 'referendum', 'congress',  'senate', 'local', 'cabildo' or 'EU'")

  }

  # Convert type to code
  cod_elec <-
    ifelse(type_elec == "referendum", "01",
           ifelse(type_elec == "congress", "02",
                  ifelse(type_elec == "senate", "03",
                         ifelse(type_elec == "local", "04",
                                ifelse(type_elec == "regional", "05",
                                       ifelse(type_elec == "cabildo",
                                              "06", "07"))))))

  # output
  return(cod_elec)
}


#' @title Extract region codes from the poll stations codes
#'
#' @description Extract region codes (for aggregation levels ccaa,
#' prov, mun, municipal districts and census sections) for a given
#' poll station code provided by Spanish Ministry of Interior (MIR)
#' and Spanish Statistical Office (INE)
#'
#' @param id_INE_poll_station poll station code. It should be a
#' string vector with tokens between 18 and 19 characters with 5 '-'
#' according to the INE/MIR format").
#' @param level aggregation level, for which we want to extract codes. It
#' should be taken from the following values: 'ccaa', 'prov', 'mun',
#' 'mun-district', 'sec' or 'poll-station'
#' @param full_cod flag to indicate if codes should be provided in a
#' full format (including codes of more aggregated levels) or not.
#' Defaults to \code{FALSE}.
#'
#' @return A string code subtract from the whole code the properly id
#' for the aggregation level required.
#'
#' @author Javier Alvarez-Liebana, David Pereiro-Pol, Mafalda
#' Gonzalez-Gonzalez and Irene Bosque-Gala.
#' @keywords utils
#' @name extract_code
#'
#' @examples
#'
#' ## Correct examples
#'
#' # Code for Adra ("003"), from province of Almeria ("04") and ccaa of
#' # Andalucia ("01"), first municipal district ("01"), census
#' # sections district ("004") and poll station "B"
#' id_INE_poll_station <- "01-04-003-01-004-B"
#'
#' extract_code(id_INE_poll_station, level = "mun", full_cod = FALSE)
#' extract_code(id_INE_poll_station, level = "mun", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "prov", full_cod = FALSE)
#' extract_code(id_INE_poll_station, level = "prov", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "ccaa", full_cod = FALSE)
#' extract_code(id_INE_poll_station, level = "ccaa", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "mun_district",
#'              full_cod = TRUE)
#'
#' # ----
#' # Incorrect examples
#' # ----
#'
#' \dontrun{
#' extract_code(id_INE_poll_station, level = "prov", full_cod = false)
#' extract_code(id_INE_poll_station, level = "province", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "muni", full_cod = "all")
#' extract_code(id_INE_poll_station, level = "poll", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "district", full_cod = TRUE)
#' }
#'
#' @export
extract_code <-
  function(id_INE_poll_station, level = "mun", full_cod = FALSE) {

    # Check: if id_INE_poll_station was provided in a valid format
    if (!(is.character(id_INE_poll_station) &
          all(between(nchar(id_INE_poll_station), 18, 19)) &
          length(str_extract_all(id_INE_poll_station, "-")[[1]]) == 5)) {

      stop("Argument 'id_INE_poll_station' must be a string of 18-19 characters with 5 '-' according to the INE format")
    }

    # Check: if level takes an allowed value
    if (!(level %in% c("ccaa", "prov", "mun", "mun_district",
                       "sec", "poll_station"))) {

      stop("Aggregation level provided by 'level' parameter must be taken from the following values: 'ccaa', 'prov', 'mun', 'mun-district', 'sec', 'poll_station'")

    }

    # Check: if full_codi is a logical variable
    if (!is.logical(full_cod)) {

      stop("Parameter 'full_cod' must be a logical variable, just TRUE/FALSE are avoided")

    }

    # Split id INE
    id_split <- str_split(id_INE_poll_station, "-")

    # Compute the number of elements
    i <- ifelse(level == "ccaa", 1,
             ifelse(level == "prov", 2,
                    ifelse(level == "mun", 3,
                           ifelse(level == "mun_district", 4,
                                  ifelse(level == "sec", 5, 6)))))

    # Access to elements of the list
    if (full_cod) {

      cod <- id_split |>
        map_chr(function(x) {
          str_c(x[1:i], collapse = "-")
          })

    } else {

      cod <-
        id_split |>
        map_chr(function(x) {
          x[i]
          })

    }

    # Output
    return(cod)
}

recod_mun <- function(mun_data) {

  mun_data <-
    mun_data |>
    mutate("cod_INE_mun" =
             case_when(cod_INE_prov == "04" & cod_INE_mun == "025" ~ "029",
                       cod_INE_prov == "04" & cod_INE_mun == "039" ~ "007",
                       cod_INE_prov == "05" & cod_INE_mun == "050" ~ "903",
                       cod_INE_prov == "22" & cod_INE_mun == "005" ~ "907",
                       cod_INE_prov == "16" & cod_INE_mun == "260" ~ "910",
                       cod_INE_prov == "19" & cod_INE_mun == "012" ~ "269",
                       cod_INE_prov == "19" & cod_INE_mun == "276" ~ "256",
                       cod_INE_prov == "05" &
                         cod_INE_mun %in% c("004", "028", "146", "250", "255") ~ "019",
                       cod_INE_prov == "09" & cod_INE_mun == "031" ~ "902",
                       cod_INE_prov == "09" & cod_INE_mun == "158" ~ "174",
                       cod_INE_prov == "09" & cod_INE_mun == "263" ~ "905",
                       cod_INE_prov == "09" & cod_INE_mun == "278" ~ "048",
                       cod_INE_prov == "09" & cod_INE_mun == "364" ~ "905",
                       cod_INE_prov == "09" & cod_INE_mun == "475" ~ "463",
                       cod_INE_prov == "24" & cod_INE_mun == "072" ~ "064",
                       cod_INE_prov == "37" & cod_INE_mun == "084" ~ "185",
                       cod_INE_prov == "37" & cod_INE_mun == "093" ~ "185",
                       cod_INE_prov == "24" & cod_INE_mun == "111" ~ "130",
                       cod_INE_prov == "49" & cod_INE_mun == "074" ~ "264",
                       cod_INE_prov == "17" & cod_INE_mun == "122" ~ "096",
                       cod_INE_prov == "15" & cod_INE_mun == "026" ~ "902",
                       cod_INE_prov == "15" & cod_INE_mun == "063" ~ "902",
                       cod_INE_prov == "27" & cod_INE_mun == "036" ~ "901",
                       cod_INE_prov == "36" & cod_INE_mun == "011" ~ "902",
                       cod_INE_prov == "36" & cod_INE_mun == "012" ~ "902",
                       cod_INE_prov == "01" & cod_INE_mun == "026" ~ "901",
                       cod_INE_prov == "12" & cod_INE_mun == "066" ~ "902",
                       cod_INE_prov == "13" & cod_INE_mun == "099" ~ "901",
                       cod_INE_prov == "09" & cod_INE_mun == "080" ~ "048",
                       TRUE ~ cod_INE_mun),
           "cod_INE_prov" = if_else(cod_INE_prov == "12" &
                                      cod_INE_mun == "902", "46", cod_INE_prov))

  return(mun_data)

}

#' @title Recoding Spanish political party or candidacies names and
#' preprocessing of acronyms
#'
#' @description This function standardizes and recodes the names and
#' abbreviations of Spanish political party names in a given dataset.
#' It handles various text transformations, such as removing special
#' characters, trimming whitespace, and converting characters to ASCII.
#' The function also recodes certain party names and abbreviations to
#' standardized formats based on predefined patterns.
#'
#' @param parties_data A dataset (preferably a tibble) containing
#' political party data. Function will expect a dataset with 2 columns
#' for party acronyms and full names.
#' @param col_acronym A character string specifying the column name
#' for party acronyms in `parties_data`. Defaults to "abbrev_candidacies".
#' @param col_full_name A character string specifying the column name
#' for party full names in `parties_data`. Defaults to "name_candidacies".
#'
#' @return A tibble with standardized and recoded political party
#' names and acronyms. The output datasets includes cleaned versions
#' of the original columns and recoded values based on predefined
#' patterns.
#'
#' @details The function first renames the specified columns in the
#' input dataset to a standard format. It then performs a series of
#' text cleaning operations, such as removing special characters,
#' replacing certain symbols with hyphens, and converting accented
#' characters to their ASCII equivalents. After cleaning the text,
#' the function applies a series of pattern-matching rules to recode
#' specific party names and abbreviations to standardized versions.
#'
#' @author Javier Alvarez-Liebana, David Pereiro-Pol, Mafalda
#' Gonzalez-Gonzalez and Irene Bosque-Gala.
#' @keywords utils
#' @name recod_parties
#'
#' @examples
#' \dontrun{
#' # Assuming `party_data` is a dataframe with columns
#' # "abbrev" and "full_name"
#' standardized_data <-
#'   recod_parties(party_data, col_name_abbrev = "abbrev",
#'                 col_name_candidacies = "full_name")
#' }
#'
#' @export
utils::globalVariables(c("abbrev_candidacies", "name_candidacies"))
recod_parties <-
  function(parties_data, col_acronym = "abbrev_candidacies", col_full_name = "name_candidacies") {

    # Check if col_acronym and col_full_name are characters
    if (!is.character(col_acronym) ||
        !is.character(col_full_name)) {

      stop("Parameters 'col_acronym' and 'col_full_name' must be character")
    }

    # Check if parties_data contains at least two required columns
    if (!(col_acronym %in% names(parties_data) & col_full_name %in% names(parties_data))) {

      stop("Data must contain two column with the same names as indicated in 'col_acronym' and 'col_full_name'")

    }

    # Convert to tibble
    parties_data <- as_tibble(parties_data)

    # Rename acronym and full name columns
    parties_data <-
      parties_data |>
      rename(abbrev_candidacies = col_acronym, name_candidacies = col_full_name)

    # General Cleanup
    parties_data <-
      parties_data |>
      # Remove quotes
      mutate("abbrev_candidacies" = str_remove_all(abbrev_candidacies, "'|\\.|\\,|´"),
             "abbrev_candidacies" = str_remove_all(abbrev_candidacies, '\\"')) |>
      # Trimming. Reformat -. Translate to Latin-ASCIII (remove tildes)
      mutate("abbrev_candidacies" = str_squish(abbrev_candidacies),
             "abbrev_candidacies" = str_replace_all(abbrev_candidacies, "–| - |/", "-"),
             "abbrev_candidacies" = str_replace_all(abbrev_candidacies, "\\+", ""),
             "abbrev_candidacies" = stri_trans_general(abbrev_candidacies, "Latin-ASCII")) |>
      # same for full names
      mutate("name_candidacies" = str_remove_all(name_candidacies, "'|\\.|\\,|´"),
             "name_candidacies" = str_remove_all(name_candidacies, '\\"'),
             "name_candidacies" = str_squish(name_candidacies),
             "name_candidacies" = str_replace_all(name_candidacies, "–| - |/", "-"),
             "name_candidacies" = str_replace_all(name_candidacies, "\\+", ""),
             "name_candidacies" = stri_trans_general(name_candidacies, "Latin-ASCII"))

    # Recoding full names
    parties_data <-
      parties_data |>
      mutate("name_candidacies" = str_to_upper(name_candidacies),
             "name_candidacies" =
               case_when(
                 str_detect(name_candidacies, "COMPROMISO POR GALICIA") ~ "OTROS",
                 str_detect(name_candidacies, "JUNTS PER CATALUNYA") ~ "JUNTS PER CATALUNYA",
                 str_detect(name_candidacies, "CANDIDATURA DUNITAT POPULAR-PER LA RUPTURA|CUP-PR") ~ "CANDIDATURA UNITAT POPULAR",
                 str_detect(name_candidacies, "EN COMU PODEM|UNIDOS PODEMOS|UNIDAS PODEMOS|UNIDES PODEM|ELKARREKIN PODEMOS|UNITS PODEM|PODEMOS|EN MAREA") ~ "UNIDAS PODEMOS",
                 str_detect(name_candidacies, "IZQU UNIDA-ESQUERRA UNIDA DEL PAIS VALENCIA|IZQUIERDA UNIDA-CONVOCATORIA POR ANDALUCIA|IZQUIERDA UNIDA|IULV-CA|ICV-EUIA") ~ "IZQUIERDA UNIDA",
                 str_detect(name_candidacies, "CENTRE DEMOCRATIC I SOCIAL|CENTRO DEMOCRATICO Y SOCIAL") ~ "CENTRO DEMOCRATICO Y SOCIAL",
                 str_detect(name_candidacies, "SUMAR") ~ "SUMAR",
                 str_detect(name_candidacies, "POR AVILA") ~ "POR AVILA",
                 str_detect(name_candidacies, "ACCION POR CEUTA") ~ "ACCION POR CEUTA",
                 str_detect(name_candidacies, "COALICION POR MELILLA") ~ "COALICION POR MELILLA",
                 str_detect(name_candidacies, "ASOCIACION DE ELECTORES DE CEUTA|ADEC") ~ "ASOCIACION DE ELECTORES DE CEUTA",
                 str_detect(name_candidacies, "UNION DEL PUEBLO NAVARRO") ~ "UNION DEL PUEBLO NAVARRO",
                 str_detect(name_candidacies, "PARTIDO POPULAR CENTRISTAS DE GALICIA|PARTIDO POPULAR|PARTIT POP|PP") ~ "PARTIDO POPULAR",
                 str_detect(name_candidacies, "NUEVA CANARIAS|COALICION CANARIA|NC-CCA-PNC|NC-CC-PNC|NC-CCA|NC-CC|CC-NC-PNC|CC-PNC|CCA-PNC") ~ "COALICION CANARIA-NUEVA CANARIAS",
                 str_detect(name_candidacies, "CIUTADANS PARTIDO DE LA CIUDADANIA|CIUTADANS-PARTIDO DE LA CIUDADANIA|CIUDADANOS-PARTIDO DE LA CIUDADANIA|CIUDADANOS PARTIDO DE LA CIUDADANIA") ~ "CIUDADANOS",
                 str_detect(name_candidacies, "MES COMPROMIS|COMPROMIS") ~ "COMPROMIS",
                 str_detect(name_candidacies, "PSOE-PROGR|PART SOCIALISTA OBRERO ESPANOL DE ANDALUCIA|PART SOCIALISTA OBRERO ESPANOL DE ANDALUCIA|SOCIALISTES|PARTIT DELS SOCIALISTES|PARTIT SOCIALISTA OBRER ESPANYOL|PARTIDO DOS SOCIALISTAS|PARTIDO SOCIALISTA|SOCIALISTA OBRERO ESPANOL") ~ "PARTIDO SOCIALISTA OBRERO ESPANOL",
                 str_detect(name_candidacies, "EHBILDU|EH BILDU") ~ "EH-BILDU",
                 str_detect(name_candidacies, "!TERUEL EXISTE|TERUEL EXISTE|TERUELEXISTE") ~ "TERUEL EXISTE",
                 str_detect(name_candidacies, "ALIANZA POP|ALIANZA POP|ALIANZA POPULAR|UPN-AP") ~ "ALIANZA POPULAR",
                 str_detect(name_candidacies, "BLOQUE NACIONALISTA GALEGO|BNG-NOS|BNG-NOS") ~ "BLOQUE NACIONALISTA GALEGO",
                 str_detect(name_candidacies, "AVANT ADELANTE LOS VERDES|AVANT LOS VERDES|GREENS|LOS VERDES|LV-LV|AVANT-LOS V|VERDES") ~ "LOS VERDES",
                 str_detect(name_candidacies, "DEMOCRACIA I LLIBERTAT|DEMOCRACIA Y LIBERTAD") ~ "DEMOCRACIA Y LIBERTAD",
                 str_detect(name_candidacies, "M PAIS|MAS PAIS") ~ "MAS PAIS",
                 str_detect(name_candidacies, "PARTIDO ANDALUCISTA|PSA-PA") ~ "PARTIDO ANDALUCISTA",
                 str_detect(name_candidacies, "ESQUERRA REPUBLICANA|ESQUERRA REPUBLICANA DE CATALUNYA-CATALUNYA SI|ERC-CATSI|ERC") ~ "ESQUERRA REPUBLICANA DE CATALUNYA",
                 str_detect(name_candidacies, "RUIZ-MATEOS") ~ "RUIZ-MATEOS",
                 str_detect(name_candidacies, "EUSKO ALKARTASUNA|EA-EUE") ~ "EUSKO ALKARTASUNA",
                 str_detect(name_candidacies, "INICIATIVA PER CATALUNYA") ~ "INICIATIVA PER CATALUNYA",
                 str_detect(name_candidacies, "NAVARRA SUMA|NA\\+") ~ "NAVARRA SUMA",
                 str_detect(name_candidacies, "LIGA INTERNACIONAL DE TRABAJADORES|LIT-CI|LITCI") ~ "LIGA INTERNACIONAL DE TRABAJADORES",
                 TRUE ~ "OTROS"))

    # Recoding abbrev_candidaciess
    parties_data <-
      parties_data |>
      mutate("abbrev_candidacies" = str_to_upper(abbrev_candidacies),
             "abbrev_candidacies" =
               case_when(
                 str_detect(name_candidacies, "OTROS") ~ "OTROS",
                 abbrev_candidacies == "PPSO" ~ "OTROS",
                 abbrev_candidacies == "CUPS" ~ "OTROS",
                 str_detect(name_candidacies, "JUNTS PER CATALUNYA") |
                   str_detect(abbrev_candidacies, "JXCAT") ~ "JXCAT",
                 str_detect(name_candidacies, "COMPROMISO POR GALICIA") ~ "OTROS",
                 str_detect(abbrev_candidacies, "PPMAJO-UP") ~ "OTROS",
                 str_detect(abbrev_candidacies, "CUP|CUP-PR") |
                   str_detect(name_candidacies, "CANDIDATURA UNITAT POPULAR") ~ "CUP",
                 str_detect(abbrev_candidacies, "UP|PODEMOS|ECP|EN MAREA") |
                   str_detect(name_candidacies, "UNIDAS PODEMOS|UNIDOS PODEMOS|PODEM") ~ "UP",
                 str_detect(abbrev_candidacies, "IU|IZQUIERDA UNIDA|IULV-CA|ICV-EUIA") |
                   str_detect(name_candidacies, "IZQUIERDA UNIDA") ~ "IU",
                 str_detect(abbrev_candidacies, "CDS|CENTRO DEMOCRATICO Y SOCIAL") |
                   str_detect(name_candidacies, "CENTRO DEMOCRATICO Y SOCIAL") ~ "CDS",
                 str_detect(abbrev_candidacies, "SUMAR") | str_detect(name_candidacies, "SUMAR") ~ "SUMAR",
                 str_detect(abbrev_candidacies, "XAV") |
                   str_detect(name_candidacies, "POR AVILA") ~ "XAV",
                 str_detect(abbrev_candidacies, "APC") |
                   str_detect(name_candidacies, "ACCION POR CEUTA") ~ "APC",
                 str_detect(abbrev_candidacies, "CPM") |
                   str_detect(name_candidacies, "COALICION POR MELILLA") ~ "CPM",
                 str_detect(abbrev_candidacies, "ADEC") | str_detect(name_candidacies, "ASOCIACION DE ELECTORES DE CEUTA") ~ "ADEC",
                 str_detect(abbrev_candidacies, "UPN|UNION DEL PUEBLO NAVARRO") |
                   str_detect(name_candidacies, "UNION DEL PUEBLO NAVARRO") ~ "UPN",
                 str_detect(abbrev_candidacies, "PP|PARTIDO POPULAR") |
                   str_detect(name_candidacies, "PARTIDO POPULAR") ~ "PP",
                 str_detect(abbrev_candidacies, "CC|NC|PNC") |
                   str_detect(name_candidacies, "COALICION CANARIA-NUEVA CANARIAS") ~ "CC-NC",
                 str_detect(abbrev_candidacies, "CIUTADANS|CIUDADANOS") |
                   str_detect(name_candidacies, "CIUDADANOS") ~ "CS",
                 str_detect(abbrev_candidacies, "COMPROMIS") |
                   str_detect(name_candidacies, "MES COMPROMIS|COMPROMIS") ~ "COMPROMIS",
                 str_detect(abbrev_candidacies, "PSOE|PROGR|PSG|PSC|PSE") |
                   str_detect(name_candidacies, "PARTIDO SOCIALISTA OBRERO ESP") ~ "PSOE",
                 str_detect(abbrev_candidacies, "PARTIDO POPULAR|PP") |
                   str_detect(name_candidacies, "PARTIDO POPULAR") ~ "PP",
                 str_detect(abbrev_candidacies, "EHBILDU|EH BILDU") |
                   str_detect(name_candidacies, "EHBILDU|EH BILDU")~ "EH-BILDU",
                 str_detect(abbrev_candidacies, "TERUEL EXISTE | TE") |
                   str_detect(name_candidacies, "TERUEL EXISTE") ~ "TE",
                 str_detect(abbrev_candidacies, "ALIANZA POPULAR|AP") |
                   str_detect(name_candidacies, "ALIANZA POPULAR") ~ "AP",
                 str_detect(abbrev_candidacies, "BNG|BNG-NOS") |
                   str_detect(name_candidacies, "BLOQUE NACIONALISTA GALEGO") ~ "BNG",
                 str_detect(abbrev_candidacies, "LV-LV|LOS VERDES|VERDES")|
                   str_detect(name_candidacies, "LOS VERDES") ~ "LV",
                 str_detect(abbrev_candidacies, "DL|DIL") |
                   str_detect(name_candidacies, "DEMOCRACIA Y LIBERTAD") ~ "DIL",
                 str_detect(abbrev_candidacies, "COMPROMIS") |
                   str_detect(name_candidacies, "COMPROMIS") ~ "COMPROMIS",
                 str_detect(abbrev_candidacies, "MP") |
                   str_detect(name_candidacies, "MAS PAIS") ~ "MP",
                 str_detect(abbrev_candidacies, "PSA-PA") |
                   str_detect(name_candidacies, "PARTIDO ANDALUCISTA") ~ "PSA-PA",
                 str_detect(abbrev_candidacies, "ERC") |
                   str_detect(name_candidacies, "ESQUERRA REPUBLICANA") ~ "ERC",
                 str_detect(abbrev_candidacies, "ARM") |
                   str_detect(name_candidacies, "RUIZ-MATEOS") ~ "ARM",
                 str_detect(abbrev_candidacies, "EA-EUE") |
                   str_detect(name_candidacies, "EUSKO ALKARTASUNA") ~ "EA-EUE",
                 str_detect(abbrev_candidacies, "ICV") |
                   str_detect(name_candidacies, "INICIATIVA PER CATALUNYA") ~ "ICV",
                 str_detect(abbrev_candidacies, "NA-SUMA") |
                   str_detect(name_candidacies, "NAVARRA SUMA") ~ "NA-SUMA",
                 str_detect(abbrev_candidacies, "LIT-CI|LITCI") |
                   str_detect(name_candidacies, "LIGA INTERNACIONAL DE TRABAJADORES") ~ "LIT-CI",
                 TRUE ~ "OTROS"))

    return(parties_data)

  }





