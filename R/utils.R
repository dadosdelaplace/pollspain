# Load required libraries
#check, install if necessary, and load the required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, stringr, stringi, glue, tibble, tidyr, purr, lubridate)
###########################################################


#' @title Conversion type election to code
#'
#' @description Conversion the type of election (referendum,
#' congress, etc) to a properly code (according to documentation)
#' available in Spanish Ministry of Interior
#'
#' @param type_elec type elections for which data is available.
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
#' @author Mikaela DeSmedt;Javier Alvarez-Liebana
#' @source Data extracted from
#' \url{https://infoelectoral.interior.gob.es/opencms/es/elecciones-celebradas/area-de-descargas/}{Spanish Ministry of Interior}
#' @keywords utils
#' @name type_to_code_election
#'
#' @examples
#' ## Convert type to code
#'
#' # Right examples
#' type_to_code_election(type_elec = "congress")
#' type_to_code_election(type_elec = "senate")
#' type_to_code_election(type_elec = "local")
#'
#' \dontrun{
#' # Wrong examples
#' type_to_code_election(type_elec = "hi!")
#' type_to_code_election(type_elec = "nop")
#' type_to_code_election(type_elec = "national")
#' }
#'
#' @export
# Function to map election type to code
type_to_code_election <- function(type_elec) {
  # Check: if value in type_elec is allowed
  if (!all(type_elec %in% c("referendum", "congress", "senate", "local", "regional", "cabildo", "EU"))) {
    stop("Input argument type_elec is not valid: it must be taken one of the following values: 'referendum', 'congress', 'senate', 'local', 'regional', 'cabildo' or 'EU'")
  }

  # Convert type to code and directory
  info <- switch(type_elec,
                 "referendum" = list(dir = "01-referendum", cod_elec = "01"),
                 "congress" = list(dir = "02-congress", cod_elec = "02"),
                 "senate" = list(dir = "03-senate", cod_elec = "03"),
                 "local" = list(dir = "04-local", cod_elec = "04"),
                 "regional" = list(dir = "05-regional", cod_elec = "05"),
                 "cabildo" = list(dir = "06-cabildo", cod_elec = "06"),
                 "EU" = list(dir = "07-EU", cod_elec = "07"))

  # Output the information
  return(info)
}



#' @title Extract region codes from the poll stations codes
#'
#' @description Extract region codes (for aggregation levels ccaa, prov, mun,
#' municipal districts and census sections) for a given poll station code
#' provided by Spanish Ministry of Interior (MIR) and Spanish Statistical
#' Office (INE)
#'
#' @param id_INE_poll_station poll station code. It should be a string vector
#' with tokens between 18 and 19 characters with 5 '-' according to the INE/MIR
#' format").
#' @param level aggregation level, for which we want to extract codes. It
#' should be taken from the following values: 'ccaa', 'prov', 'mun',
#' 'mun-district', 'sec' or 'poll-station'
#' @param full_cod flag to indicate if codes should be provided in a full format
#' (including codes of more aggregated levels) or not. Defaults to
#' \code{FALSE}.
#'
#' @return A string code subtract from the whole code the properly id for the
#' aggregation level required.
#'
#' @author Javier Alvarez-Liebana
#' @keywords utils
#' @name extract_code
#'
#' @examples
#'
#' ## Extracting codes
#'
#' # Code for Adra ("003"), from province of Almeria ("04") and ccaa of
#' # Andalucia ("01"), first municipal district ("01"), census sections district
#' # ("004") and poll station "B"
#' id_INE_poll_station <- "01-04-003-01-004-B"
#'
#' # Right examples
#' extract_code(id_INE_poll_station, level = "mun", full_cod = FALSE)
#' extract_code(id_INE_poll_station, level = "mun", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "prov", full_cod = FALSE)
#' extract_code(id_INE_poll_station, level = "prov", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "ccaa", full_cod = FALSE)
#' extract_code(id_INE_poll_station, level = "ccaa", full_cod = TRUE)
#' extract_code(id_INE_poll_station, level = "mun_district", full_cod = TRUE)
#'
#' \dontrun{
#' # Wrong examples
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


#' @title Recode Political Party Names and Abbreviations
#'
#' @description This function standardizes and recodes the names and abbreviations of political parties in a given dataset. It handles various text transformations, such as removing special characters, trimming whitespace, and converting characters to ASCII. The function also recodes certain party names and abbreviations to standardized formats based on predefined patterns.
#'
#' @param parties_data A dataframe containing political party data with columns for party abbreviations and full names.
#' @param col_name_abbrev A character string specifying the column name for party abbreviations in the input dataframe. Defaults to "abbrev_candidacies".
#' @param col_name_candidacies A character string specifying the column name for party full names in the input dataframe. Defaults to "name_candidacies".
#'
#' @return A dataframe with standardized and recoded political party names and abbreviations. The output dataframe includes cleaned versions of the original columns and recoded values based on predefined patterns.
#'
#' @details The function first renames the specified columns in the input dataframe to a standard format ("abbrev_candidacies" and "name_candidacies"). It then performs a series of text cleaning operations, such as removing special characters, replacing certain symbols with hyphens, and converting accented characters to their ASCII equivalents. After cleaning the text, the function applies a series of pattern-matching rules to recode specific party names and abbreviations to standardized versions.
#'
#' @author Mikaela DeSmedt;Javier Alvarez-Liebana
#'
#' @examples
#' \dontrun{
#' # Assuming `party_data` is a dataframe with columns "abbrev" and "full_name"
#' standardized_data <- recod_parties(party_data, col_name_abbrev = "abbrev", col_name_candidacies = "full_name")
#' }
#'
#' @import dplyr stringr stringi
#' @export


recod_parties <- function(parties_data, col_name_abbrev = "abbrev_candidacies", col_name_candidacies = "name_candidacies") {

  # Rename abbreviation and name columns
  parties_data <- parties_data %>%
    rename(abbrev_candidacies = {{col_name_abbrev}},
           name_candidacies = {{col_name_candidacies}})

  # General Cleanup
  parties_data <- parties_data %>%
    mutate(
      abbrev_candidacies = str_remove_all(abbrev_candidacies, "'|\\.|\\,|´"),
      abbrev_candidacies = str_remove_all(abbrev_candidacies, '\\"'),
      abbrev_candidacies = str_trim(abbrev_candidacies),
      abbrev_candidacies = str_replace_all(abbrev_candidacies, "–| - |/", "-"),
      abbrev_candidacies = str_replace_all(abbrev_candidacies, "\\+", ""),
      abbrev_candidacies = stri_trans_general(abbrev_candidacies, "Latin-ASCII"),

      name_candidacies = str_remove_all(name_candidacies, "'|\\.|\\,|´"),
      name_candidacies = str_remove_all(name_candidacies, '\\"'),
      name_candidacies = str_trim(name_candidacies),
      name_candidacies = str_replace_all(name_candidacies, "–| - |/", "-"),
      name_candidacies = str_replace_all(name_candidacies, "\\+", ""),
      name_candidacies = stri_trans_general(name_candidacies, "Latin-ASCII")
    )

  # Apply recoding using case_when
  parties_data <- parties_data %>%
    mutate(
      name_candidacies = case_when(
        str_detect(name_candidacies, "IZQU UNIDA-ESQUERRA UNIDA DEL PAIS VALENCIA|IZQUIERDA UNIDA-CONVOCATORIA POR ANDALUCIA|^IZQUIERDA UNIDA") ~ "IZQUIERDA UNIDA",
        str_detect(name_candidacies, "^CENTRE DEMOCRATIC I SOCIAL|^CENTRO DEMOCRATICO Y SOCIAL") ~ "CENTRO DEMOCRATICO Y SOCIAL",
        str_detect(name_candidacies, "^PARTIDO COMUNISTA") ~ "PARTIDO COMUNISTA DE ESPANA",
        str_detect(name_candidacies, "SUMAR") ~ "SUMAR",
        str_detect(name_candidacies, "UNION DEL PUEBLO NAVARRO-PARTIDO POPULAR|UNION DEL PUEBLO NAVARRO EN COALICION CON EL PARTIDO|PARTIDO POPULAR CENTRISTAS DE GALICIA|UNION DEL PUEBLO NAVARRO CON PARTIDO POPULAR|UNIÓN DEL PUEBLO NAVARRO EN COALICIÓN CON EL PARTI|PARTIDO POPULAR|PARTIT POP|PP EN COALICION CON UPM|UNION DEL PUEBLO NAVARRO EN COALICION CON EL PARTI") ~ "PARTIDO POPULAR",
        str_detect(name_candidacies, "COALICION CANARIA-PARTIDO NACIONALISTA CANARIO|COALICION CANARIA-NUEVA CANARIAS|NUEVA CANARIAS-COALICION CANARIA") ~ "COALICION CANARIA-NUEVA CANARIAS",
        str_detect(name_candidacies, "CIUTADANS PARTIDO DE LA CIUDADANIA|CIUTADANS-PARTIDO DE LA CIUDADANIA|CIUDADANOS-PARTIDO DE LA CIUDADANIA|CIUDADANOS PARTIDO DE LA CIUDADANIA") ~ "CIUDADANOS",
        str_detect(name_candidacies, "MES COMPROMIS") ~ "MES COMPROMIS",
        str_detect(name_candidacies, "EN COMU PODEM-GUANYEM EL CANVI|EN COMUN-UNIDAS PODEMOS|UNIDAS PODEMOS|UNIDES PODEM|ELKARREKIN PODEMOS-UNIDAS PODEMOS|UNITS PODEM MES|COMPROMIS-PODEMOS-EUPV: A LA VALENCIANA|EN MAREA|PODEMOS") ~ "UNIDAS PODEMOS",
        str_detect(name_candidacies, "PART SOCIALISTA OBRERO ESPANOL DE ANDALUCIA|PART SOCIALISTA OBRERO ESPANOL DE ANDALUCIA|SOCIALISTES|PARTIT DELS SOCIALISTES|PARTIT SOCIALISTA OBRER ESPANYOL|^PARTIDO DOS SOCIALISTAS|^PARTIDO SOCIALISTA") ~ "PARTIDO SOCIALISTA OBRERO ESPANOL",
        str_detect(name_candidacies, "CANDIDATURA DUNITAT POPULAR-PER LA RUPTURA|CUP-PR") ~ "CANDIDATURA DUNITAT POPULAR-PER LA RUPTURA",
        str_detect(name_candidacies, "EHBILDU|EH BILDU") ~ "EH-BILDU",
        str_detect(name_candidacies, "!TERUEL EXI") ~ "TERUEL EXISTE",
        str_detect(name_candidacies, "ALIANZA POP-PDEMOCPOPULAR-UNION VALENCIANA|ALIANZA POP-PDEMOCPOPULAR-PDEMOCLIBERAL-UCD|ALIANZA POPULAR|UPN-AP") ~ "ALIANZA POPULAR",
        str_detect(name_candidacies, "ARA|PV") ~ "ARA REPUBLIQUES",
        str_detect(name_candidacies, "BLOQUE NACIONALISTA GALEGO|BNG-NOS|BNG-NOS") ~ "BLOQUE NACIONALISTA GALEGO",
        str_detect(name_candidacies, "NC-CCA-PNC|NC-CC-PNC|NC-CCA|NC-CC|CC-NC-PNC|CC-PNC|CCA-PNC|COALICION CANARIA-NUEVA CANARIAS|NUEVA CANARIAS-COALICION CANARIA") ~ "COALICION CANARIA-NUEVA CANARIAS",
        str_detect(name_candidacies, "COMPROMIS") ~ "COMPROMIS",
        str_detect(name_candidacies, "AVANT ADELANTE LOS VERDES|AVANT LOS VERDES|GREENS|LOS VERDES|LV-LV|AVANT-LOS V|VERDES") ~ "LOS VERDES",
        str_detect(name_candidacies, "DL") ~ "CONVERGENCIA I UNIO",
        str_detect(name_candidacies, "M PAIS|MAS PAIS") ~ "MAS PAIS",
        str_detect(name_candidacies, "IULV-CA|ICV-EUIA") ~ "IZQUIERDA UNIDA",
        str_detect(name_candidacies, "PSA-PA") ~ "PARTIDO ANDALUCISTA",
        str_detect(name_candidacies, "ESQUERRA REPUBLICANA|ESQUERRA REPUBLICANA DE CATALUNYA-CATALUNYA SI|ERC-CATSI|ERC") ~ "ESQUERRA REPUBLICANA",
        str_detect(name_candidacies, "RUIZ-MATEOS") ~ "RUIZ-MATEOS",
        str_detect(name_candidacies, "EA-EUE") ~ "EUSKO ALKARTASUNA",
        str_detect(name_candidacies, "NA\\+") ~ "NAVARRA SUMA",
        str_detect(name_candidacies, "LIT-CI|LITCI") ~ "LIGA INTERNACIONAL DE TRABAJADORES",
        TRUE ~ name_candidacies
      ),
      abbrev_candidacies = case_when(
        str_detect(name_candidacies, "IZQUIERDA UNIDA") ~ "IU",
        str_detect(name_candidacies, "CENTRO DEMOCRATICO Y SOCIAL") ~ "CDS",
        str_detect(name_candidacies, "^PARTIDO COMUNISTA") ~ "PCE",
        str_detect(name_candidacies, "SUMAR") ~ "SUMAR",
        str_detect(name_candidacies, "COALICION CANARIA-NUEVA CANARIAS|NUEVA CANARIAS-COALICION CANARIA") ~ "CC-NC",
        str_detect(name_candidacies, "CIUTADANS-PARTIDO DE LA CIUDADANIA|CIUDADANOS-PARTIDO DE LA CIUDADANIA|CIUDADANOS") ~ "CS",
        str_detect(name_candidacies, "MES COMPROMIS") ~ "MES",
        str_detect(name_candidacies, "EN COMUN-UNIDAS PODEMOS|UNIDAS PODEMOS|UNIDES PODEM|ELKARREKIN PODEMOS-UNIDAS PODEMOS|UNITS PODEM MES|COMPROMIS-PODEMOS-EUPV: A LA VALENCIANA|EN MAREA|^PODEMOS") ~ "UP",
        str_detect(name_candidacies, "PARTIDO DE LOS SOCIALISTAS DE GALICIA-PSOE|^PARTIDO SOCIALISTA|SOCIALISTES|PARTIT DELS SOCIALISTES|PARTIT SOCIALISTA OBRER ESPANYOL|PARTIDO DOS SOCIALISTAS DE GALICIA-PARTIDO SOCIALI|PARTIDO SOCIALISTA DE EUSKADI-EUSKADIKO EZKERRA \\(P\\)") ~ "PSOE",
        str_detect(name_candidacies, "CANDIDATURA DUNITAT POPULAR-PER LA RUPTURA|CUP-PR") ~ "CUP",
        str_detect(name_candidacies, "PARTIDO POPULAR") ~ "PP",
        str_detect(name_candidacies, "EHBILDU|EH BILDU") ~ "EH-BILDU",
        str_detect(name_candidacies, "!TERUEL EXI") ~ "TE",
        str_detect(name_candidacies, "ALIANZA POPULAR|AP") ~ "AP",
        str_detect(name_candidacies, "ARA|PV") ~ "ARA-PV",
        str_detect(name_candidacies, "BLOQUE NACIONALISTA GALEGO|BNG-NOS|BNG-NOS|NOS") ~ "BNG",
        str_detect(name_candidacies, "NC-CCA-PNC|NC-CC-PNC|NC-CCA|NC-CC|CC-NC-PNC|CC-PNC|CCA-PNC|COALICION CANARIA-NUEVA CANARIAS|NUEVA CANARIAS-COALICION CANARIA") ~ "CC-NC",
        str_detect(name_candidacies, "COMPROMIS") ~ "COMPROMIS",
        str_detect(name_candidacies, "AVANT ADELANTE LOS VERDES|AVANT LOS VERDES|GREENS|LOS VERDES|LV-LV|AVANT-LOS V|VERDES") ~ "LV",
        str_detect(name_candidacies, "DL") ~ "DIL-CDC",
        str_detect(name_candidacies, "M PAIS|MAS PAIS") ~ "MP",
        str_detect(name_candidacies, "IULV-CA|ICV-EUIA") ~ "IU",
        str_detect(name_candidacies, "PSA-PA") ~ "PA",
        str_detect(name_candidacies, "ESQUERRA REPUBLICANA DE CATALUNYA-SOBIRANISTES|ESQUERRA REPUBLICANA") ~ "ERC",
        str_detect(name_candidacies, "RUIZ-MATEOS") ~ "ARM",
        str_detect(name_candidacies, "EA-EUE") ~ "EA",
        str_detect(name_candidacies, "NA\\+") ~ "NA-SUMA",
        str_detect(name_candidacies, "PARTIDO POPULAR") ~ "PP",
        str_detect(name_candidacies, "LIT-CI|LITCI") ~ "LIT-CI",
        TRUE ~ abbrev_candidacies
      )
    )

  return(parties_data)
}


