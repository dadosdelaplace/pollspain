
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
#' @author Javier Álvarez-Liébana.
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
#' @author Javier Álvarez-Liébana.
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
#' extract_code(id_INE_poll_station, level = "mun-district", full_cod = TRUE)
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
    if (!(level %in% c("ccaa", "prov", "mun", "mun-district",
                       "sec", "poll-station"))) {

      stop("Aggregation level provided by 'level' parameter must be taken from the following values: 'ccaa', 'prov', 'mun', 'mun-district', 'sec', 'poll-station'")

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
                           ifelse(level == "mun-district", 4,
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


#' @title Recode of party's names
#'
#' @description ...
#'
#' @param parties_data ...
#' @param col_name_abbrev ...
#' @param col_name_candidacies ...
#'
#' @return ...
#'
#' @author Javier Álvarez-Liébana.
#' @keywords utils
#' @name recod_parties
#' @export
recod_parties <-
  function(parties_data, col_name_abbrev = "abbrev_candidacies",
           col_name_candidacies = "name_candidacies") {

    # Check: if col_name_abbrev and col_name_candidacies are characters
    if (!is.character(col_name_abbrev) ||
        !is.character(col_name_candidacies)) {

      stop("Parameters 'col_name_abbrev' and 'col_name_candidacies' must be character")
    }
    # Check: if parties_data contains at least two required columns
    if (!(col_name_abbrev %in% names(parties_data))) {

      stop("Data must contain (at least) a column with abbrev of candidacies")

    }

    # Rename
    parties_data <-
      parties_data |>
      rename(abbrev_candidacies = col_name_abbrev)

    # Recode
    parties_data <-
      parties_data |>
      mutate(
      # Remove ' and . and ,. Trimming. Reformat -
      abbrev_candidacies = str_remove_all(abbrev_candidacies, "'|\\.|\\,|´"),
      abbrev_candidacies = str_remove_all(abbrev_candidacies, '\\"'),
      abbrev_candidacies = str_trim(abbrev_candidacies),
      abbrev_candidacies = str_replace_all(abbrev_candidacies, "–| - |/", "-"),
      abbrev_candidacies = str_replace_all(abbrev_candidacies, "\\+", ""),
      # Remove tildes just from abbrev
      abbrev_candidacies =
        stri_trans_general(abbrev_candidacies, "Latin-ASCII"))

    # Preproc abbrev
    parties_data <-
      parties_data |>
      mutate(
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "EHBILDU|EH BILDU", "EH-BILDU"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "!TERUEL EXI", "TE"),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "AHORA CANARIAS"), "AHORA CANARIAS",
               abbrev_candidacies),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies,
                        "AP-PDP-PL-C|AP-PL-C|AP-PDP-PDL-|AP-PDP-PAR|AP-PDP-PDL|AP-PDP-PL|AP-PDP-UV|AP-PL-UPN|AP-PDP|AP-PAR|AP-PDL|AP-UV|AP-PL",
                        "AP"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "ARA, PV", "ARA-PV"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "BNG-NÓS|BNG-NOS|NÓS|NOS", "BNG"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "CC-PNC|CCA-PNC", "CC"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "NC-CCA-PNC|NC-CC-PNC|NC-CCA|NC-CC|CC-NC-PNC", "CC-NC"),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "COMPROMIS"),
               "COMPROMIS", abbrev_candidacies),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "AVANT ADELANTE LOS VERDES|AVANT LOS VERDES|GREENS|LOS VERDES|LV-LV|AVANT-LOS V|VERDES",
                        "LV"),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "CUP-PR", "CUP"),
      abbrev_candidacies = ifelse(str_detect(abbrev_candidacies, "GREENS"),
                                  "LV", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(abbrev_candidacies == "DL", "DIL-CDC", abbrev_candidacies),
      abbrev_candidacies =
        str_replace_all(abbrev_candidacies, "M PAIS|MAS PAIS", "MP"),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies,
                          "PARTIT DELS SOCIALISTES DE CATALUNYA|PARTIDO DOS SOCIALISTAS DE GALICIA"),
               "PSOE", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies,
                          "PARTIDO SOCIALISTA OBRERO ESPAÑOL DE ANDALUCIA|PART. SOCIALISTA OBRERO ESPAÑOL DE ANDALUCIA"),
               "PSOE", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "PSE-EE|PARTIDO SOCIALISTA DE ARAGON|PARTIDO DOS SOCIALISTAS DE GALICIA"),
               "PSOE", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "PSOE-PROGR.|PSOE-PROGR|PSOE|PSC|PSE"),
               "PSOE", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "IULV-CA|ICV-EUIA"),
               "IU", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "PSA-PA"), "PA", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "ERC-CATSI|ERC"),
               "ERC", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "RUIZ-MATEOS"),
               "ARM", abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "EA-EUE"), "EA",
               abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "NA\\+"), "NA-SUMA",
               abbrev_candidacies),
      abbrev_candidacies =
        ifelse(str_detect(abbrev_candidacies, "LIT-CI|LITCI"),
               "LIT-CI", abbrev_candidacies))

    # Preproc names
    if (col_name_candidacies %in% names(parties_data)) {

      # Rename
      parties_data <-
        parties_data |>
        rename(name_candidacies = col_name_candidacies)

      parties_data <-
        parties_data |>
        mutate(
          name_candidacies = str_replace_all(name_candidacies, "–| - |/", "-"),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "AHORA CANARIAS"), "AHORA CANARIAS",
                   abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(abbrev_candidacies, "DL") &
                     str_detect(name_candidacies, "LLIBERTAT"),
                   "DIL-CDC", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "LLUITA INTERNACIONALISTA"),
                   "LIT-CI", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "RECORTES CERO-GRUPO VERDE"),
                   "RECORTES CERO-LV", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "BERDEAK-LOS VERDES"),
                   "BERDEAK-LV", abbrev_candidacies),
          abbrev_candidacies = ifelse(str_detect(name_candidacies, "GREENS"),
                                      "LV", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "UNIDAS PODEMOS|UNIDOS PODEMOS"),
                   "UP", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies,
                              "PARTIT DELS SOCIALISTES DE CATALUNYA|PARTIDO DOS SOCIALISTAS DE GALICIA"),
                   "PSOE", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies,
                              "PARTIDO SOCIALISTA OBRERO ESPAÑOL DE ANDALUCIA|PART. SOCIALISTA OBRERO ESPAÑOL DE ANDALUCIA"),
                   "PSOE", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "PSE-EE|PARTIDO SOCIALISTA DE ARAGON|PARTIDO DOS SOCIALISTAS DE GALICIA"),
                   "PSOE", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(abbrev_candidacies, "PSOE-PROGR.|PSOE-PROGR"),
                   "PSOE", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "PARTIDO SOCIALISTA DE ANDALUCIA-PARTIDO ANDALUZ"),
                   "PA", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies,
                              "ESQUERRA REPUBLICANA DE CATALUNYA"),
                   "ERC", abbrev_candidacies),
          name_candidacies =
            ifelse(str_detect(name_candidacies, "UNIDAS PODEMOS|UNIDOS PODEMOS"),
                   "UNIDOS PODEMOS", name_candidacies),
          name_candidacies =
            ifelse(abbrev_candidacies == "AP",
                   "ALIANZA POPULAR-COALICION POPULAR", name_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "NAVARRA SUMA"), "NA-SUMA",
                   abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies, "RUIZ-MATEOS"),
                   "ARM", abbrev_candidacies),
          abbrev_candidacies =
            ifelse(str_detect(name_candidacies,
                              "INICIATIVA PER CATALUNYA-ELS VERDS"),
                   "ICV", abbrev_candidacies))

    }

    return(parties_data)

}
