
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
summary_data_all <- summary_election_data(type_elec = "congress", year = 2019, month = 4)
summary_data_all
```

| id_elec | blank_ballots | invalid_ballots | party_ballots | valid_ballots | total_ballots | n_poll_stations | pop_res_all | census_counting_all |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| 02-2019-04-28 | 199836 | 276769 | 26001535 | 26084449 | 26359783 | 60090 | 46722980 | 34799420 |

``` r
# Summary election data at prov level, aggregating the candidacies ballots
summary_data_prov_parties <- summary_election_data(type_elec = "congress", year = 2019, month = 4, level = "prov",  by_parties = TRUE)
head(summary_data_prov_parties, 5)
```

| id_elec | cod_INE_ccaa | cod_INE_prov | blank_ballots | invalid_ballots | party_ballots | valid_ballots | total_ballots | n_poll_stations | id_candidacies | abbrev_candidacies | name_candidacies | ballots | pop_res_prov | census_counting_prov | porc_candidacies_parties | porc_candidacies_valid | porc_candidacies_census |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|:---|:---|:---|---:|---:|---:|---:|---:|---:|
| 02-2019-04-28 | 01 | 04 | 2283 | 2918 | 325814 | 326582 | 329496 | 804 | 000117 | VOX | VOX | 62648 | 709340 | 458996 | 19.228 | 19.183 | 13.649 |
| 02-2019-04-28 | 01 | 04 | 2283 | 2918 | 325814 | 326582 | 329496 | 804 | 000028 | EB | ESCAÑOS EN BLANCO | 536 | 709340 | 458996 | 0.165 | 0.164 | 0.117 |
| 02-2019-04-28 | 01 | 04 | 2283 | 2918 | 325814 | 326582 | 329496 | 804 | 000104 | RECORTES CE | RECORTES CERO-GRUPO VERDE | 412 | 709340 | 458996 | 0.126 | 0.126 | 0.090 |
| 02-2019-04-28 | 01 | 04 | 2283 | 2918 | 325814 | 326582 | 329496 | 804 | 000083 | PP | PARTIDO POPULAR | 73952 | 709340 | 458996 | 22.698 | 22.644 | 16.112 |
| 02-2019-04-28 | 01 | 04 | 2283 | 2918 | 325814 | 326582 | 329496 | 804 | 000096 | PSOE | PARTIDO SOCIALISTA OBRERO ESPAÑOL | 98924 | 709340 | 458996 | 30.362 | 30.291 | 21.552 |

To do:

- explicar bien argumentos y usos (combina, de manera sencilla para el
  usuario, `get_election_data()` y `aggregate_election_data()`)
- solo año (con opción a dar fecha completa)
- by_parties y short version por defecto
- duplicates 1982 y 1986

<details>
<summary><strong>Advanced users</strong></summary>

Poner una aperitivo y referirle a los articles.

</details>

### Seat allocation

<details>

<summary>

<strong>Advanced users</strong>
</summary>

Poner una aperitivo y referirle a los articles.

</details>

### Resúmenes de encuestas

<details>

<summary>

<strong>Advanced users</strong>
</summary>

Poner una aperitivo y referirle a los articles.

</details>

### Estimación de encuestas

<details>

<summary>

<strong>Advanced users</strong>
</summary>

Poner una aperitivo y referirle a los articles.

</details>

### Simulación de resultados electorales

<details>

<summary>

<strong>Advanced users</strong>
</summary>

Poner una aperitivo y referirle a los articles.

</details>

### Data viz

- barras ordenadas a más a menos (con colores)
- ggparlament
- encuestas + promedio
- barras con resultados + encuestas encima
- barras con % de voto vs %escaños?
- mapa
- ¿algún lollipop para mostrar housing efects? con flechas y eso.

<details>

<summary>

<strong>Advanced users</strong>
</summary>

Poner una aperitivo y referirle a los articles.

</details>

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
#> 🔎 Check if parameters are allowed...
#> 📦 Import poll station data from ...
#> - https://github.com/dadosdelaplace/pollspain-data/blob/main/02-congress/02201904/raw_poll_stations_congress_2019_04.rda?raw=true
#> - https://github.com/dadosdelaplace/pollspain-data/blob/main/02-congress/02201911/raw_poll_stations_congress_2019_11.rda?raw=true
#> 🔎 Check if parameters are allowed...
#> 📦 Import census mun data from ...
#> - https://github.com/dadosdelaplace/pollspain-data/blob/main/02-congress/02201904/raw_mun_data_congress_2019_04.rda?raw=true
#> - https://github.com/dadosdelaplace/pollspain-data/blob/main/02-congress/02201911/raw_mun_data_congress_2019_11.rda?raw=true
#> ⚠️ A short version was asked. If you require all variables, please run with `short_version = FALSE'
head(poll_data)
#> # A tibble: 6 × 20
#>   id_elec  type_elec date_elec  id_INE_poll_station id_INE_mun ccaa  prov  mun  
#>   <glue>   <chr>     <date>     <glue>              <glue>     <chr> <chr> <chr>
#> 1 02-2019… congress  2019-04-28 01-04-003-01-004-B  01-04-003  Anda… Alme… Adra 
#> 2 02-2019… congress  2019-04-28 01-04-003-01-007-U  01-04-003  Anda… Alme… Adra 
#> 3 02-2019… congress  2019-04-28 01-04-003-02-001-A  01-04-003  Anda… Alme… Adra 
#> 4 02-2019… congress  2019-04-28 01-04-006-01-002-A  01-04-006  Anda… Alme… Albox
#> 5 02-2019… congress  2019-04-28 01-04-008-01-001-A  01-04-008  Anda… Alme… Alcó…
#> 6 02-2019… congress  2019-04-28 01-04-010-01-001-U  01-04-010  Anda… Alme… Alha…
#> # ℹ 12 more variables: blank_ballots <dbl>, invalid_ballots <dbl>,
#> #   party_ballots <dbl>, valid_ballots <dbl>, total_ballots <dbl>,
#> #   turnout <dbl>, porc_valid <dbl>, porc_invalid <dbl>, porc_parties <dbl>,
#> #   porc_blank <dbl>, pop_res_mun <dbl>, census_counting_mun <dbl>
```

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
