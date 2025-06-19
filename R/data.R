#' @title Dates of Spanish elections
#'
#' @description A dataset containing the dates of Spanish
#' elections in referendum, congress, senate, municipal, cabildo
#' (Canarian council) and European Parlament elections. Only
#' elections from 1980 onwards have been provided. Last update:
#' 2025/05/01.
#'
#' @format A tibble with 62 rows and 7 variables:
#' \itemize{
#'   \item \code{cod_elec}: code of type of elections. Allowed values:
#'   \itemize{
#'     \item \code{"01"}: referendum elections.
#'     \item \code{"02"}: congress elections.
#'     \item \code{"03"}: senate elections.
#'     \item \code{"04"}: local (municipalities) elections.
#'     \item \code{"05"}: regional  elections.
#'     \item \code{"06"}: cabildo - Canarian council - elections).
#'     \item \code{"07"}: European Parlament elections.
#'   }
#'   \item \code{type_elec}: type of elections ("referendum",
#'   "congress", "senate", "local", "regional", "cabildo" or "EU").
#'   \item \code{date}: date of election in "YYYY-MM-DD" format
#'   \item \code{year}, \code{month}, \code{day}: year, month and
#'   day of election
#'   \item \code{topic}: topic of referendums (\code{cod_elec = "01"})
#' }
#'
#' @author Javier Alvarez-Liebana and David Pereiro-Pol.
#'
#' @source Data extracted from
#' \href{https://infoelectoral.interior.gob.es/opencms/es/elecciones-celebradas/area-de-descargas/}{Spanish Ministry of Interior}
#' @docType data
#' @keywords datasets
#' @name dates_elections_spain
#' @usage data(dates_elections_spain)
#'
#' @examples
#' # Load data
#' data(dates_elections_spain)
"dates_elections_spain"

#' @title INE's code for ccaa and provinces
#'
#' @description A dataset containing the codes provided by INE
#' of Spanish provinces and regions. Last update: 2023/02/25
#'
#' @format A tibble with 52 rows and 5 variables:
#' \itemize{
#'   \item \code{cod_INE_ccaa}: code of region according INE.
#'   \item \code{cod_MIR_ccaa}: code of region according Spanish
#'   Ministry of the Interior (MIR).
#'   \item \code{ccaa}: name of region
#'   \item \code{cod_INE_prov}: code of province
#'   \item \code{prov}: name of province
#' }
#'
#' @author Javier Alvarez-Liebana and David Pereiro-Pol.
#'
#' @source Data extracted from
#' \href{https://www.ine.es/daco/daco42/codmun/cod_ccaa_provincia.htm}{INE}
#' @docType data
#' @keywords datasets
#' @name cod_INE_prov_ccaa
#' @usage data(cod_INE_prov_ccaa)
#'
#' @examples
#' # Load data
#' data(cod_INE_prov_ccaa)
"cod_INE_prov_ccaa"

#' @title INE's code for ccaa and provinces
#'
#' @description A dataset containing the codes provided by INE
#' of Spanish provinces and regions. Last update: 2023/02/25
#'
#' @format A tibble with 52 rows and 2 variables:
#' \itemize{
#'   \item \code{cod_INE_prov}: code of province
#'   \item \code{prov}: name of province
#' }
#'
#' @author Javier Alvarez-Liebana and David Pereiro-Pol.
#'
#' @source Data extracted from
#' \href{https://www.ine.es/daco/daco42/codmun/cod_ccaa_provincia.htm}{INE}
#' @docType data
#' @keywords datasets
#' @name cod_INE_prov
#' @usage data(cod_INE_prov)
#'
#' @examples
#' # Load data
#' data(cod_INE_prov)
"cod_INE_prov"

#' @title INE's code for ccaa and provinces
#'
#' @description A dataset containing the codes provided by INE
#' of Spanish provinces and regions. Last update: 2023/02/25
#'
#' @format A tibble with 19 rows and 2 variables:
#' \itemize{
#'   \item \code{cod_INE_ccaa}: code of regions
#'   \item \code{ccaa}: name of regions
#' }
#'
#' @author Javier Alvarez-Liebana and David Pereiro-Pol.
#'
#' @source Data extracted from
#' \href{https://www.ine.es/daco/daco42/codmun/cod_ccaa_provincia.htm}{INE}
#' @docType data
#' @keywords datasets
#' @name cod_INE_ccaa
#' @usage data(cod_INE_ccaa)
#'
#' @examples
#' # Load data
#' data(cod_INE_ccaa)
"cod_INE_ccaa"

#' @title INE's code for municipalities
#'
#' @description A dataset containing the codes provided by INE
#' of Spanish municipalities. Last update: 2025/05/03 (data
#' updated by INE on 2020/01/01)
#'
#' @format A tibble with 8131 rows and 10 variables:
#' \itemize{
#'   \item \code{id_INE_ccaa, id_MIR_ccaa}: full id of municipalities
#'   (combining ccaa, province and mun's id)
#'   \item \code{cod_INE_ccaa, cod_MIR_ccaa}: code of regions
#'   \item \code{ccaa}: name of regions
#'   \item \code{cod_INE_prov}: code of provinces
#'   \item \code{prov}: name of provinces
#'   \item \code{cod_INE_mun}: code of municipalities
#'   \item \code{cd_INE_mun}: check digit (see \href{https://www.ine.es/daco/daco42/codmun/codmun00i.htm}{documentation})
#'   \item \code{mun}: name of municipalities
#' }
#'
#' @author Javier Alvarez-Liebana and David Pereiro-Pol.
#'
#' @source Data extracted from \href{https://www.ine.es/daco/daco42/codmun/codmun20/20codmun.xlsx}{INE}
#' @docType data
#' @keywords datasets
#' @name cod_INE_mun
#' @usage data(cod_INE_mun)
#'
#' @examples
#' # Load data
#' data(cod_INE_mun)
"cod_INE_mun"


#' @title Dictionary for parties and candidacies
#'
#' @description A dataset containing, for each election, the
#' abbreviation and name for each candidacy, as well as the
#' national id and province's id. An hexadecimal code is proposed
#' for the most important candidacies for dataviz purposes.
#'
#' @format A tibble with 7 columns:
#' \itemize{
#'   \item \code{id_elec}: id of elections (type of elections + date).
#'   \item \code{abbrev_candidacies}: party abbreviation.
#'   \item \code{name_candidacies}: party name (at each constituency).
#'   \item \code{id_candidacies_nat}: national id.
#'   \item \code{id_candidacies}: id at each constituency.
#'   \item \code{name_candidacies_nat}: party name (common at
#'   national level).
#'   \item \code{color}: hexadecimal (color) code for some parties.
#' }
#'
#' @author Irene Bosque-Gala, Mafalda Gonzalez-Gonzalez and Javier
#' Alvarez-Liebana.
#'
#' @source Data collected from all the election files.
#' @docType data
#' @keywords datasets
#' @name global_dict_parties
#' @usage data(global_dict_parties)
#'
#' @examples
#' # Load data
#' data(global_dict_parties)
"global_dict_parties"

#' @title Election results according to Central Election Board
#'
#' @description A dataset with the data contained on the PDF
#' documents provided since 2004 by Central Election Board (CEB) in
#' Spain. This dataset is used in the unit tests since the data
#' provided by the Ministry of the Interior are not always corrected
#' by the CEB data (which are always at the province level,
#' not below). The tests check that such discrepancies do not exceed
#' 0.5% of the votes.
#'
#' @format A tibble with 15 columns
#' \itemize{
#'   \item \code{id_elec}: id of elections (type of elections + date).
#'   \item \code{cod_elec}: code of elections.
#'   \item \code{date}: date of elections.
#'   \item \code{cod_INE_ccaa, cod_MIR_ccaa, ccaa}: codes and names
#'   for ccaa.
#'   \item \code{cod_INE_prov, id_INE_prov, prov}: codes and names
#'   for provinces.
#'   \item \code{census_counting_prov}: census for each election
#'   and province.
#'   \item \code{total_ballots}: total number of ballots, as a sum of
#'   \code{valid_ballots} and \code{invalid_ballots}.
#'   \item \code{valid_ballots}: total number of valid ballots, as a
#'   sum of \code{party_ballots} and \code{blank_ballots}.
#'   \item \code{party_ballots}: total number of ballots to
#'   candidacies.
#'   \item \code{blank_ballots}: total number of blank ballots.
#'   \item \code{invalid_ballots}: total number of invalid ballots.
#' }
#'
#' @author Javier Alvarez-Liebana.
#'
#' @source Data was scraped from the PDF documents provided since
#' 2004 by Central Election Board (CEB) in Spain. See
#' \url{https://www.juntaelectoralcentral.es/cs/jec/elecciones}
#' @docType data
#' @keywords datasets
#' @name CEB_results
#' @usage data(CEB_results)
#'
#' @examples
#' # Load data
#' data(CEB_results)
"CEB_results"

#' @title Number of parliament seats allocated to each province for
#' the general Spanish elections.
#'
#' @description A dataset containing the parliament seats for each
#' electoral district (province) and election, just consdering
#' congress seats.
#'
#' @format A tibble with 8 columns
#' \itemize{
#'   \item \code{id_elec}: id of elections (type of elections + date).
#'   \item \code{cod_INE_ccaa, cod_MIR_ccaa, ccaa}: codes and names
#'   for ccaa.
#'   \item \code{cod_INE_prov, prov}: codes and names for provinces.
#'   \item \code{id_INE_prov}: id for prov (`cod_INE_ccaa` +
#'   `cod_INE_prov`)
#'   \item \code{nseats}: number of seats allocated to each province.
#' }
#'
#' @author Javier Alvarez-Liebana and David Pereiro-Pol.
#'
#' @source Data extracted from
#' \url{https://www.juntaelectoralcentral.es/cs/jec/elecciones}
#' @docType data
#' @keywords datasets
#' @name total_seats_spain
#' @usage data(total_seats_spain)
#'
#' @examples
#' # Load data
#' data(total_seats_spain)
"total_seats_spain"

