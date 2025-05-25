
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
#' 'mun-district', 'sec' or 'poll_station'
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

      stop("Aggregation level provided by 'level' parameter must be taken from the following values: 'ccaa', 'prov', 'mun', 'mun_district', 'sec', 'poll_station'")

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

#' @title Recode municipalities
#'
#' @description Given a dataset with municipality data, this function
#' transparently returns the updated code based on that recoding,
#' reassigning codes for merge municipalities accordingly.
#'
#' @param mun_data a tibble with info about municipalities.
#'
#' @details The municipality data (names and codes) were extracted
#' from the version published by the National Statistics Institute
#' (INE) on February 6, 2025. Over the years, various municipal
#' mergers have taken place in Spain, which means that not all
#' elections feature the same set of municipalities or the same
#' identifying codes. In order to unify and standardize the results
#' provided to users, all output tables refer to the most recent
#' municipality recoding by the Spanish National Statistics Institute
#' (INE).
#'
#' @author Javier Alvarez-Liebana.
#' @keywords utils
#' @name recod_mun
#'
#' @export
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





