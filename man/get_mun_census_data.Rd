% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_elections_data.R
\name{get_mun_census_data}
\alias{get_mun_census_data}
\title{Get municipal census data}
\source{
Some definitions of variables were extracted from
\url{https://www.ige.gal}
}
\usage{
get_mun_census_data(type_elec, year, month)
}
\arguments{
\item{type_elec}{type elections for which data is available.
It should be one of the following values: "referendum",
"congress", "senate", "local", "cabildo" (Canarian council)
or "EU".}

\item{year, month}{vector of years and months of elections to be considered.
Please be sure (see \code{dates_elections_spain}) that elections of the
provided type are available for the given year and month.}
}
\value{
A tibble (as many rows as municipalities for each election) with the
following elements
\item{cod_elec}{code of type of elections: \code{"01"} (referendum),
\code{"02"} (congress), \code{"03"} (senate), \code{"04"} (local elections),
\code{"06"} (cabildo - Canarian council - elections), \code{"07"}
(European Parlament elections)}
\item{type_elec}{type of election.}
\item{date_elec}{date of election.}
\item{id_INE_mun}{municipality's id build from ccaa-prov-mun codes provided
by INE.}
\item{id_MIR_mun}{municipality's id build from ccaa-prov-mun codes provided
by Spanish Ministry of Interior (MIR).}
\item{cod_INE_ccaa, cod_MIR_ccaa, ccaa}{codes and name for regions (ccaa)
to which it belongs municipalities.}
\item{cod_INE_prov, prov}{codes and name for provinces to which it belongs
municipalities.}
\item{cod_INE_mun, cd_INE_mun, mun}{code, digit control and name for
municipalities.}
\item{cod_mun_jud_district, cod_mun_prov_council}{codes of judicial
district and province council.}
\item{n_poll_stations}{number of poll stations at each mun}
\item{pop_res_mun}{census of people who are living (CER + CERA).}
\item{census_INE_mun}{people from \code{pop_res_mun} who are
allowed to vote.}
\item{census_counting_mun}{people from \code{census_INE_mun} after claims.}
\item{census_CERE_mun}{ensus of foreigners, just for EU elections.}
}
\description{
Get municipal census data for a particular election at municipal
level. This function is a wrapper of \code{import_raw_mun_MIR_files()}
function for a set of elections
}
\examples{

## Get mun census data
data("dates_elections_spain")
\dontrun{
# Right examples
mun_census_data <- get_mun_census_data("congress", 2019, 4)
mun_census_data <- get_mun_census_data("senate", 2019, 11)
mun_census_data <- get_mun_census_data(rep("congress", 3),
                                       c(2019, 2019, 2016),
                                       c(11, 4, 6))
mun_census_data <- get_mun_census_data(c("congress", "senate"),
                                       c(2019, 2019), c(11, 4))
# Wrong examples
mun_census_data <- get_mun_census_data("national", 2019, 4)
mun_census_data <- get_mun_census_data("congress", 2016, c(4, 11))
mun_census_data <- get_mun_census_data("congress", "2016-06-26")
}

}
\author{
Javier Álvarez-Liébana.
}
\keyword{get_elections_data}
