
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


