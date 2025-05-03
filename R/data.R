#' @title Dates of Spanish elections
#'
#' @description A dataset containing the dates of Spanish
#' elections in referendum, congress, senate, municipal, cabildo
#' (Canarian council) and European Parlament elections. Only
#' elections from 1980 onwards have been provided. Last update:
#' 2025/05/01.
#'
#' @format A data frame with 61 rows and 7 variables:
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
#' @author Javier Álvarez-Liébana and David Pereiro-Pol.
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
#' @format A data frame with 52 rows and 4 variables:
#' \itemize{
#'   \item \code{cod_INE_ccaa}: code of region
#'   \item \code{ccaa}: name of region
#'   \item \code{cod_INE_prov}: code of province
#'   \item \code{prov}: name of province
#' }
#'
#' @author Javier Álvarez-Liébana and David Pereiro-Pol.
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
#' @format A data frame with 52 rows and 2 variables:
#' \itemize{
#'   \item \code{cod_INE_prov}: code of province
#'   \item \code{prov}: name of province
#' }
#'
#' @author Javier Álvarez-Liébana and David Pereiro-Pol.
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
#' @format A data frame with 19 rows and 2 variables:
#' \itemize{
#'   \item \code{cod_INE_ccaa}: code of regions
#'   \item \code{ccaa}: name of regions
#' }
#'
#' @author Javier Álvarez-Liébana and David Pereiro-Pol.
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
#' @format A data frame with 8131 rows and 7 variables:
#' \itemize{
#'   \item \code{cod_INE_ccaa}: code of regions
#'   \item \code{ccaa}: name of regions
#'   \item \code{cod_INE_prov}: code of provinces
#'   \item \code{prov}: name of provinces
#'   \item \code{cod_INE_mun}: code of municipalities
#'   \item \code{cd_INE_mun}: check digit (see \href{https://www.ine.es/daco/daco42/codmun/codmun00i.htm}{documentation})
#'   \item \code{mun}: name of municipalities
#' }
#'
#' @author Javier Álvarez-Liébana and David Pereiro-Pol.
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





