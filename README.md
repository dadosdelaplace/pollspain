
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![GitHub
release](https://img.shields.io/github/v/release/dadosdelaplace/pollspain)](https://github.com/dadosdelaplace/pollspain/releases)
<!-- badges: end -->

# pollspain

The main objective of the R package `{pollspain}` is to provide social
scientists, political analysts, journalists, and citizens with easy and
straightforward **access to electoral data from Spain**. This includes
both **election results** extracted from polling stations (**aggregated
by party and/or geographic level**) and **electoral survey data**
(including the associated margins of error). The package also offers
simple tools for **seat allocation**, **vote simulation**, and **results
visualization**.

The package is designed so that users with basic knowledge of `R` can
use it, providing **tidyverse-style functions**.

## Installation

You can **install the development version** of R package `{pollspain}`
from [GitHub](https://github.com/) through the following code (please,
note that you need to install `{devtools}` package before):

``` r
install.packages("devtools") # only if not already installed
devtools::install_github("dadosdelaplace/pollspain")
```

``` r
# after installing
library(pollspain)
```

An **internet connection** is required for the installation, as well as
for downloading data.

### Available information sources

All the data is stored in the accompanying `pollspain-data` repository.
You can find the data dictionary and more information about the data
structure at <https://github.com/dadosdelaplace/pollspain-data>

## How to use?

### Election summaries

The **main function** is
`summary_election_data(type_elec = ..., year = ..., level = ...)`, which
given

- a vector of **election dates** (e.g., `year = 2023`),

- a vector of **election types** (currently, it only works properly for
  `type_elec = "congress"`),

- a geographic **level for aggregation** (e.g., `level = "ccaa"`),

returns a **summary table of election results aggregated at the
administrative level** specified in `level` argument. This includes both
general data (blank votes, turnout, etc) and the **number of votes
received by each party or candidacy**. The available **aggregation
levels** (`level`) are: `"all"` (for a national summary), `"ccaa"`
(autonomous communities), `"prov"` (province), `"mun"` (municipality),
`"mun_district"` (electoral district), `"sec"` (census section), and
`"poll_station"` (raw results at the polling station level, without
aggregation).

``` r
# Summary election data at national level (general data without candidacies ballots)
summary_data_all <-
  summary_election_data(type_elec = "congress", year = 2019, month = 4)
summary_data_all
```

| id_elec | blank_ballots | invalid_ballots | party_ballots | valid_ballots | total_ballots | n_poll_stations | pop_res_all | census_counting_all |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| 02-2019-04-28 | 199836 | 276769 | 26001535 | 26084449 | 26359783 | 60090 | 46722980 | 34799420 |

``` r
# Summary election data at ccaa level, aggregating the candidacies
# ballots, for three elections
summary_data_ccaa_parties <-
  summary_election_data("congress", 2019, 4, level = "ccaa",
                        by_parties = TRUE)
head(summary_data_ccaa_parties, 7)
```

| id_elec | cod_INE_ccaa | blank_ballots | invalid_ballots | party_ballots | valid_ballots | total_ballots | n_poll_stations | id_candidacies | abbrev_candidacies | name_candidacies | ballots | pop_res_ccaa | census_counting_ccaa | porc_candidacies_parties | porc_candidacies_valid | porc_candidacies_census |
|:---|:---|---:|---:|---:|---:|---:|---:|:---|:---|:---|---:|---:|---:|---:|---:|---:|
| 02-2019-04-28 | 01 | 43084 | 61295 | 4540994 | 4569140 | 4630265 | 10134 | 000117 | VOX | VOX | 612921 | 8384408 | 6316975 | 13.498 | 13.414 | 9.703 |
| 02-2019-04-28 | 01 | 43084 | 61295 | 4540994 | 4569140 | 4630265 | 10134 | 000028 | EB | ESCAÑOS EN BLANCO | 1324 | 8384408 | 6316975 | 0.029 | 0.029 | 0.021 |
| 02-2019-04-28 | 01 | 43084 | 61295 | 4540994 | 4569140 | 4630265 | 10134 | 000104 | RECORTES CE | RECORTES CERO-GRUPO VERDE | 7826 | 8384408 | 6316975 | 0.172 | 0.171 | 0.124 |
| 02-2019-04-28 | 01 | 43084 | 61295 | 4540994 | 4569140 | 4630265 | 10134 | 000083 | PP | PARTIDO POPULAR | 787384 | 8384408 | 6316975 | 17.339 | 17.233 | 12.465 |
| 02-2019-04-28 | 01 | 43084 | 61295 | 4540994 | 4569140 | 4630265 | 10134 | 000096 | PSOE | PARTIDO SOCIALISTA OBRERO ESPAÑOL | 1568682 | 8384408 | 6316975 | 34.545 | 34.332 | 24.833 |
| 02-2019-04-28 | 01 | 43084 | 61295 | 4540994 | 4569140 | 4630265 | 10134 | 000081 | PODEMOS-IU | UNIDAS PODEMOS | 654944 | 8384408 | 6316975 | 14.423 | 14.334 | 10.368 |
| 02-2019-04-28 | 01 | 43084 | 61295 | 4540994 | 4569140 | 4630265 | 10134 | 000022 | CS | CIUDADANOS-PARTIDO DE LA CIUDADANÍA | 811562 | 8384408 | 6316975 | 17.872 | 17.762 | 12.847 |

``` r
# Summary election data at prov level, aggregating the candidacies ballots
summary_data_prov_parties <-
  summary_election_data(type_elec = "congress", year = 2019,
                        month = 4, level = "prov",
                        by_parties = TRUE)
head(summary_data_prov_parties, 10)
```

To do:

- explicar bien argumentos y usos (combina, de manera sencilla para el
  usuario, `get_election_data()` y `aggregate_election_data()`)
- solo año (con opción a dar fecha completa)
- by_parties y short version por defecto
- duplicates 1982 y 1986

#### Usuarios avanzados

Poner una aperitivo y referirle a los articles.

### Reparto de escaños

#### Usuarios avanzados

Poner una aperitivo y referirle a los articles.

### Resúmenes de encuestas

#### Usuarios avanzados

Poner una aperitivo y referirle a los articles.

### Estimación de encuestas

#### Usuarios avanzados

Poner una aperitivo y referirle a los articles.

### Simulación de resultados electorales

#### Usuarios avanzados

Poner una aperitivo y referirle a los articles.

### Data viz

- barras ordenadas a más a menos (con colores)
- ggparlament
- encuestas + promedio
- barras con resultados + encuestas encima
- barras con % de voto vs %escaños?
- mapa
- ¿algún lollipop para mostrar housing efects? con flechas y eso.

#### Usuarios avanzados

Poner una aperitivo y referirle a los articles.

## Other functions

The ´{pollspain}\` package also provides to un usuario más avanzado some
useful functions to preprocess and analyze electoral data (even your own
electoral data siempre y cuando they are provided in a properly format).

- **Utils**: functions contained in the `utils.R` script are intended to
  serve as **helper functions for data preprocessing**. See \<…\> for
  more examples about how to use them.

``` r
type_to_code_election(type_elec = "congress")
#> [1] "02"
extract_code("01-04-003-01-004-B", level = "mun")
#> [1] "003"
extract_code("01-04-003-01-004-B", level = "mun", full_cod = TRUE)
#> [1] "01-04-003"
```

- **Import raw data**: functions starting with `import_..._data()` (code
  can be found in the `import_elections_data.R` file) are aimed at
  importing and preprocessing as raw as possible the `.DAT` election
  files from the Spanish Ministry of Interior. All functions download
  the `.rda` files available in the Github repository . See \<…\> for
  more examples about how to use them.

``` r
# import and preprocess elections data at poll stations level for given election
# types and dates, providing variables related to turnout, blank/valid votes, etc
poll_data <- import_poll_station_data("congress", 2019, c(4, 11))
head(poll_data)
```

- …

- …

<!--
&#10;## Datos del CERA (pending)
&#10;According to the National Statistics Institute (INE):
&#10;«The electoral roll contains the registration of those who meet the requirements to be voters and are not definitively or temporarily deprived of the right to vote. The electoral roll is composed of:
&#10;* The electoral roll of Spanish citizens residing in Spain (CER).
&#10;* The electoral roll of Spanish citizens residing abroad (CERA).
&#10;The electoral roll of residents in Spain who are nationals of countries with Agreements for municipal elections (CERE Agreements), and the electoral roll of citizens of the European Union residing in Spain for municipal and European Parliament elections (CERE EU).»
&#10;The `get_CERA_data()` function returns the data related to the CERA.
&#10;
``` r
ccaa_CERA_data <- get_CERA_data(election_data, level = "ccaa")
```
&#10;-->

## Authors

**Javier Álvarez-Liébana (maintainer)**, **David Pereiro-Pol**,
**Mafalda González-González**, **Irene Bosque-Gala** and **Mikaela De
Smedt**.

`{pollspain}` package ha sido una parte de varios Trabajos Fin de Máster
del Máster de Ciencias de Datos Computacionales de la UC3M (Madrid). The
**usability and funcionality** of package has been tested by the
following collaborators:

## References

This package has been designed based on the following resources and
references

- García Guzmán P (2025). WikiBarrio: Explore Spanish socio-demographic
  data at the neighborhood level. <https://www.wikibarrio.es/>

- García Guzmán P (2025). ineAtlas: Access to Spanish Household Income
  Distribution Atlas Data. R package version 0.1.3.9000,
  <https://github.com/pablogguz/ineAtlas>

- Meleiro H (2024). infoelectoral: Download Spanish Election Results. R
  package version 1.0.2, <https://github.com/rOpenSpain/infoelectoral>

- Data download repository of the Spanish Ministry of the Interior.
  <https://infoelectoral.interior.gob.es/es/elecciones-celebradas/area-de-descargas/>
