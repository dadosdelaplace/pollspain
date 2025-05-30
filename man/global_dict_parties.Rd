% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{global_dict_parties}
\alias{global_dict_parties}
\title{Dictionary for parties and candidacies}
\format{
A tibble with 7 columns:
\itemize{
\item \code{id_elec}: id of elections (type of elections + date).
\item \code{abbrev_candidacies}: party abbreviation.
\item \code{name_candidacies}: party name (at each constituency).
\item \code{id_candidacies_nat}: national id.
\item \code{id_candidacies}: id at each constituency.
\item \code{name_candidacies_nat}: party name (common at
national level).
\item \code{color}: hexadecimal (color) code for some parties.
}
}
\source{
Data collected from all the election files.
}
\usage{
data(global_dict_parties)
}
\description{
A dataset containing, for each election, the
abbreviation and name for each candidacy, as well as the
national id and province's id. An hexadecimal code is proposed
for the most important candidacies for dataviz purposes.
}
\examples{
# Load data
data(global_dict_parties)
}
\author{
Irene Bosque-Gala, Mafalda Gonzalez-Gonzalez and Javier
Alvarez-Liebana.
}
\keyword{datasets}
