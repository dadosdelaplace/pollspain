% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_elections_data.R
\name{import_poll_station_data}
\alias{import_poll_station_data}
\title{Import poll station data}
\usage{
import_poll_station_data(
  type_elec,
  year = NULL,
  date = NULL,
  prec_round = 3,
  short_version = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{type_elec}{Type elections for which data is available.
It should be one of the following values: "referendum",
"congress", "senate", "local", "cabildo" (Canarian council)
or "EU".}

\item{year}{A vector or single value representing the years of the
elections to be considered. Please, check in
\code{dates_elections_spain} that elections of the specified type
are available for the provided year.}

\item{date}{A vector or single value representing the dates of
the elections to be considered. If date was provided, it should be
in format \%Y-\%m-\%d (e.g., '2000-01-01'). Defaults to \code{NULL}.
If no date was provided, \code{year} should be provided as
numerical variable. Please, check in \code{dates_elections_spain}
that elections of the specified type are available.}

\item{prec_round}{Rounding accuracy. Defaults to
\code{prec_round = 3}.}

\item{short_version}{Flag to indicate whether it should be returned
a short version of the data (just key variables) or not.
Defaults to \code{TRUE}.}

\item{verbose}{Flag to indicate whether detailed messages should
be printed during execution. Defaults to \code{TRUE}.}
}
\value{
A tibble with rows corresponding to municipalities for
each election, including the following variables:
\item{id_elec}{election's id constructed from the election code
\code{cod_elec} and date \code{date_elec}.}
\item{cod_elec}{code representing the type of election:
\code{"01"} (referendum), \code{"02"} (congress),
\code{"03"} (senate), \code{"04"} (local elections),
\code{"06"} (cabildo - Canarian council - elections), \code{"07"}
(European Parliament elections). Variable available only for
long version.}
\item{type_elec}{type of election.}
\item{date_elec}{date of the election.}
\item{id_INE_mun}{municipality ID constructed from the
ccaa-prov-mun codes provided by INE.}
\item{id_INE_poll_station}{poll station's id constructed from the
ccaa-prov-municipality and poll station codes.}
\item{cod_INE_ccaa, ccaa}{codes and names for regions (ccaa)
to which the municipalities belong. Codes available only for
long version.}
\item{cod_INE_prov, prov}{codes and names for provinces to which
municipalities belong. Codes available only for long version.}
\item{cod_INE_mun, mun}{code, and name for
municipalities. Codes available only for long version.}
\item{cod_mun_district, cod_sec, cod_poll_station}{codes for the
municipal district, census tract and poll station. Codes available
only for long version.}
\item{census_counting_mun}{population eligible to vote after claims
at municipality level.}
\item{ballots_1, turnout_1}{number of total ballots and turnout
percentage in the first round. Variables available only for
long version.}
\item{ballots_2, turnout_2}{number of total ballots and turnout
percentage in the second round (if applicable). Variables available
only for long version}
\item{blank_ballots, invalid_ballots}{blank and invalid ballots.}
\item{party_ballots, valid_ballots, total_ballots}{ballots to
candidacies/parties, valid ballots (sum of \code{blank_ballots} and
\code{party_ballots}) and total ballots (sum of
\code{valid_ballots} and \code{invalid_ballots}).}
\item{turnout}{final turnout percentage.}
\item{porc_valid, porc_invalid, porc_parties, porc_blank}{perc (\%)
values of \code{valid_ballots}, \code{invalid_ballots},
\code{party_ballots} and \code{blank_ballots}.}
\item{pop_res_mun}{population census of residents (CER + CERA) at
municipality level.}
}
\description{
Import and preprocess elections data at poll stations
level for given election types and dates. This function supports
both single values and vector inputs for fetching and combining
data for multiple elections at once.
}
\details{
This function fetches poll station-level data for the
specified elections by downloading the corresponding files from
\code{{pollspaindata}} and processing them into a tidy format. It
automatically handles the download, loading, and merging of data
across multiple election periods as specified by the user.
}
\examples{

## Correct examples

# Fetch poll station data for congress elections in multiple dates
# in a short version
poll_station_data <-
  import_poll_station_data(type_elec = "congress", year = 2023,
                           date = "2019-04-28")

# Fetch poll station data for congress elections in multiple dates
# in a long version
poll_station_data <-
  import_poll_station_data(type_elec = "congress",
                           date = c("2019-04-28", "2023-07-24"),
                           short_version = FALSE)

# ----
# Incorrect examples
# ----

\dontrun{
# Wrong examples

# Invalid election type: "national" is not a valid election type
import_poll_station_data(type_elec = "national", year = 2019)

# Invalid election: no congress elections are available in 2018
# Please check dataset dates_elections_spain
import_poll_station_data(type_elec = "congress", year = 2018)

# Invalid date format: date should be in \%Y-\%m-\%d format
import_poll_station_data(type_elec = "congress", date = "26-06-2016")

# Invalid short version flag: short_version should be a logical
# variable
import_poll_station_data(type_elec = "congress", year = 2019,
                         short_version = "yes")
}

}
\author{
Javier Alvarez-Liebana, David Pereiro Pol, Mafalda Gonzalez
Gonzalez, Irene Bosque Gala and Mikaela De Smedt.
}
\keyword{import_elections_data}
