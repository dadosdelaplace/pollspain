% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{dates_elections_spain}
\alias{dates_elections_spain}
\title{Dates of Spanish elections}
\format{
A data frame with 64 rows and 6 variables:
\itemize{
\item \code{cod_elec}: code of type of elections. Allowed values:
\itemize{
\item \code{"01"}: referendum elections.
\item \code{"02"}: congress elections.
\item \code{"03"}: senate elections.
\item \code{"04"}: local (municipalities) elections.
\item \code{"05"}: regional  elections.
\item \code{"06"}: cabildo - Canarian council - elections).
\item \code{"07"}: European Parlament elections.
}
\item \code{type_elec}: type of elections ("referendum", "congress",
"senate", "local", "regional", "cabildo" or "EU").
\item \code{date}: date of election in "YYYY-MM-DD" format
\item \code{year}, \code{month}, \code{day}: year, month and day of election
\item \code{topic}: topic of referendums (\code{cod_elec = "01"})
}
}
\source{
Data extracted from \href{https://infoelectoral.interior.gob.es/opencms/es/elecciones-celebradas/area-de-descargas/}{Spanish Ministry of Interior}
}
\usage{
data(dates_elections_spain)
}
\description{
A dataset containing the dates of Spanish
elections in referendum, congress, senate, municipal, cabildo
(Canarian council) and European Parlament elections.
Last update: 2023/02/25
}
\examples{
# Load data
data(dates_elections_spain)
}
\author{
Data gathered and processed by Javier Álvarez-Liébana.
}
\keyword{datasets}
