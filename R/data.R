#' Municipal Codes INE
#'
#' Dataset hosting municipality codes for all 8,132 municipalities of spain with their respective CCAA and PROV codes
#'
#' @docType data
#' @name cod_INE_mun
#' @usage data(cod_INE_mun)
#' @format A tibble with 8,132 rows and 9 columns:
#' \describe{
#'   \item{cod_INE_mun}{INE code of the municipality}
#'   \item{mun}{Name of the municipality}
#'   \item{id_INE_mun}{ID in the INE format (e.g., "01-04-001")}
#'   \item{id_MIR_mun}{ID in the MIR format (e.g., "01-04-001")}
#'   \item{cod_INE_prov}{INE code of the province}
#'   \item{prov}{Name of the province}
#'   \item{cod_INE_ccaa}{INE code of the autonomous community (CCAA)}
#'   \item{cod_MIR_ccaa}{MIR code of the autonomous community (CCAA)}
#'   \item{ccaa}{Name of the autonomous community (CCAA)}
#' }
#' @source CCAA $ PROV codes from "https://ine.es/daco/daco42/codmun/cod_provincia.htm", MUN codes from "https://ine.es/daco/daco42/codmun/diccionario24.xlsx"
#' @keywords datasets INE_codes
NULL


#' Dates of Elections in Spain
#'
#' Dataset with dates of Spanish election dates from 1982. Scrapped from source and wrangled to fit the needs of the pollspain package
#'
#' @docType data
#' @name dates_elections_spain
#' @usage data(dates_elections_spain)
#' @format A tibble with 119 rows and 7 columns:
#' \describe{
#'   \item{cod_elec}{Code of the election}
#'   \item{type_elec}{Type of election (e.g., referendum, general election)}
#'   \item{date_elec}{Date of the election}
#'   \item{year}{Year of the election}
#'   \item{month}{Month of the election}
#'   \item{day}{Day of the election}
#'   \item{topic}{Topic or description of the election}
#' }
#' @source scrapped from "https://es.wikipedia.org/wiki/Anexo:Elecciones_en_Espa%C3%B1a"
#' @keywords datasets
NULL


#' Party Colors Hex Data
#'
#' Dataset holding hexadecimal codes for spanish political parties
#'
#' @docType data
#' @name party_colors_hex
#' @usage data(party_colors_hex)
#' @format A tibble with 21 rows and 3 columns:
#' \describe{
#'   \item{abbrev_candidacies}{Abbreviation of the candidacy}
#'   \item{name_candidacies}{Full name of the candidacy}
#'   \item{party_color}{Hex color code associated with the party}
#' }
#' @source Manually sourced by Mikaela DeSmedt
#' @keywords datasets party colors
NULL



#' Seat Distribution Congress Data
#'
#' Dataset holding the number of seats distributed by each constituency from 1982 until 2023.
#'
#' @docType data
#' @name seat_distribution_congress
#' @usage data(seat_distribution_congress)
#' @format A data frame with 676 rows and 4 columns:
#' \describe{
#'   \item{prov}{Province name}
#'   \item{cod_INE_prov}{INE code of the province}
#'   \item{year}{Year of the election}
#'   \item{seats}{Number of seats allocated}
#' }
#' @source Mannualy sourced by Mikaela DeSmedt from "https://es.wikipedia.org/wiki/Circunscripciones_electorales_del_Congreso_de_los_Diputados"
#' @keywords datasets seat-distribution congress
NULL
