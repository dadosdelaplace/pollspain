
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pollspain

Under development. **Authors**: Javier Álvarez-Liébana, David
Pereiro-Pol, Mafalda González-González, Irene Bosque-Gala y Mikaela De
Smedt.

## Overview

Pending to detail

## Installation

You can install the development version of R package `{pollspain}` from
[GitHub](https://github.com/) through the following code:

``` r
install.packages("devtools") # only if not already installed
devtools::install_github("dadosdelaplace/pollspain")
```

``` r
library(tidyverse)
library(pollspain)
```

## Usage

### election data functions

El código de esta familia de funciones se puede encontrar en el script
`get_elections_data.R`. La función principal es
`summary_election_data()`: dado un vector de fechas electorales, un
vector de tipos de elecciones y un nivel geográfico (`"all"`, `"ccaa"`,
`"prov"`, …), la función devuelve al usuario una tabla de resumen de los
resultados electorales. Indicando `by_parties = TRUE` la función
devuelve los resultados para cada partido o candidatura electoral, al
nivel de agregación pedido.

Los niveles de agregación (`level`) pueden ser: `"all"`, `"ccaa"`,
`"prov"`, `"mun"`, `"mun_district"` (distrito electoral), `"sec"`
(sección censal) y `"poll_station"` (mesa electoral).

``` r
# Summary election data at national level (general data without candidacies ballots)
summary_data_all <- summary_election_data("congress", 2019, 4)
#> 🔎 Check if parameters are allowed...
#> 📦 Import data at poll station level ...
#> ⏳ Please wait, the volume of data downloaded and the internet connection may take a few seconds
#>    🗺 Aggregate data at national level ...
#> ✅🖇 Join information sources and last summaries ...
knitr::kable(summary_data_all)
```

| id_elec | blank_ballots | invalid_ballots | party_ballots | valid_ballots | total_ballots | n_poll_stations | pop_res_all | census_counting_all |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| 02-2019-04-28 | 199836 | 276769 | 26001535 | 26084449 | 26359783 | 60090 | 46722980 | 34799420 |

``` r
# Summary election data at ccaa level, aggregating the candidacies ballots
summary_data_ccaa_parties <-
  summary_election_data("congress", 2019, 4, level = "ccaa",
                        by_parties = TRUE)
#> 🔎 Check if parameters are allowed...
#> 📦 Import data at poll station level ...
#> ⏳ Please wait, the volume of data downloaded and the internet connection may take a few seconds
#>    🗺 Aggregate data at ccaa level ...
#> ✅🖇 Join information sources and last summaries ...
knitr::kable(head(summary_data_ccaa_parties, 10))
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
| 02-2019-04-28 | 01 | 43084 | 61295 | 4540994 | 4569140 | 4630265 | 10134 | 000054 | PACMA | PARTIDO ANIMALISTA CONTRA EL MALTRATO ANIMAL | 62027 | 8384408 | 6316975 | 1.366 | 1.358 | 0.982 |
| 02-2019-04-28 | 01 | 43084 | 61295 | 4540994 | 4569140 | 4630265 | 10134 | 000058 | PCPA | PARTIDO COMUNISTA DEL PUEBLO ANDALUZ | 4697 | 8384408 | 6316975 | 0.103 | 0.103 | 0.074 |
| 02-2019-04-28 | 01 | 43084 | 61295 | 4540994 | 4569140 | 4630265 | 10134 | 000100 | PUM+J | POR UN MUNDO MÁS JUSTO | 4326 | 8384408 | 6316975 | 0.095 | 0.095 | 0.068 |

``` r
# Summary election data at prov level, aggregating the candidacies ballots
summary_data_prov_parties <-
  summary_election_data("congress", 2019, 4, level = "prov",
                        by_parties = TRUE)
#> 🔎 Check if parameters are allowed...
#> 📦 Import data at poll station level ...
#> ⏳ Please wait, the volume of data downloaded and the internet connection may take a few seconds
#>    🗺 Aggregate data at prov level ...
#> ✅🖇 Join information sources and last summaries ...
```

Dicha función combina, de manera sencilla para el usuario,
`get_election_data()` y `aggregate_election_data()`

#### get_election_data()

#### aggregate_election_data()

La funciòn `aggregate_election_data()` está destinada a agregar al nivel
geográfico pedido la información electoral que ya se le proporcione.

### import functions

Functions starting with `import_..._data()` (whose code can be found in
the `import_elections_data.R` file) aimed at importing and preprocessing
as raw as possible the `.DAT` election files from the Spanish Ministry
of Interior. All functions download the `.rda` files available in the
Github repository (directly downloaded from MIR website).

#### Census data (level: mun)

`import_mun_census_data()` import municipal census data for one or more
elections at the municipal level. This function downloads and processes
«raw» municipal data files for specified elections, providing variables
related to population, census and administrative codes.

``` r
mun_census_data <- import_mun_census_data("congress", 2019, 4)
#> 🔎 Check if parameters are allowed...
#> 📦 Import census mun data from ...
#> - https://github.com/dadosdelaplace/pollspain-data/blob/main/02-congress/02201904/raw_mun_data_congress_2019_04.rda?raw=true
mun_census_data
#> # A tibble: 8,131 × 18
#>    id_elec       cod_elec type_elec date_elec  id_INE_mun cod_INE_ccaa ccaa     
#>    <glue>        <chr>    <chr>     <date>     <glue>     <chr>        <chr>    
#>  1 02-2019-04-28 02       congress  2019-04-28 01-04-001  01           Andalucía
#>  2 02-2019-04-28 02       congress  2019-04-28 01-04-002  01           Andalucía
#>  3 02-2019-04-28 02       congress  2019-04-28 01-04-003  01           Andalucía
#>  4 02-2019-04-28 02       congress  2019-04-28 01-04-004  01           Andalucía
#>  5 02-2019-04-28 02       congress  2019-04-28 01-04-005  01           Andalucía
#>  6 02-2019-04-28 02       congress  2019-04-28 01-04-006  01           Andalucía
#>  7 02-2019-04-28 02       congress  2019-04-28 01-04-007  01           Andalucía
#>  8 02-2019-04-28 02       congress  2019-04-28 01-04-008  01           Andalucía
#>  9 02-2019-04-28 02       congress  2019-04-28 01-04-009  01           Andalucía
#> 10 02-2019-04-28 02       congress  2019-04-28 01-04-010  01           Andalucía
#> # ℹ 8,121 more rows
#> # ℹ 11 more variables: cod_INE_prov <chr>, prov <chr>, cod_INE_mun <chr>,
#> #   mun <chr>, cod_mun_jud_district <chr>, cod_mun_prov_council <chr>,
#> #   n_poll_stations <dbl>, pop_res_mun <dbl>, census_INE_mun <dbl>,
#> #   census_counting_mun <dbl>, census_CERE_mun <dbl>
```

#### Poll station data (level: poll station)

`import_poll_station_data()` import and preprocess elections data at
poll stations level for given election types and dates, providing
variables related to turnout, blank/valid votes, etc

``` r
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
poll_data
#> # A tibble: 119,697 × 20
#>    id_elec type_elec date_elec  id_INE_poll_station id_INE_mun ccaa  prov  mun  
#>    <glue>  <chr>     <date>     <glue>              <glue>     <chr> <chr> <chr>
#>  1 02-201… congress  2019-04-28 01-04-003-01-004-B  01-04-003  Anda… Alme… Adra 
#>  2 02-201… congress  2019-04-28 01-04-003-01-007-U  01-04-003  Anda… Alme… Adra 
#>  3 02-201… congress  2019-04-28 01-04-003-02-001-A  01-04-003  Anda… Alme… Adra 
#>  4 02-201… congress  2019-04-28 01-04-006-01-002-A  01-04-006  Anda… Alme… Albox
#>  5 02-201… congress  2019-04-28 01-04-008-01-001-A  01-04-008  Anda… Alme… Alcó…
#>  6 02-201… congress  2019-04-28 01-04-010-01-001-U  01-04-010  Anda… Alme… Alha…
#>  7 02-201… congress  2019-04-28 01-04-013-02-005-A  01-04-013  Anda… Alme… Alme…
#>  8 02-201… congress  2019-04-28 01-04-013-03-011-A  01-04-013  Anda… Alme… Alme…
#>  9 02-201… congress  2019-04-28 01-04-013-06-013-A  01-04-013  Anda… Alme… Alme…
#> 10 02-201… congress  2019-04-28 01-04-013-06-016-B  01-04-013  Anda… Alme… Alme…
#> # ℹ 119,687 more rows
#> # ℹ 12 more variables: blank_ballots <dbl>, invalid_ballots <dbl>,
#> #   party_ballots <dbl>, valid_ballots <dbl>, total_ballots <dbl>,
#> #   turnout <dbl>, porc_valid <dbl>, porc_invalid <dbl>, porc_parties <dbl>,
#> #   porc_blank <dbl>, pop_res_mun <dbl>, census_counting_mun <dbl>
```

#### Candidacies data (level: poll station)

`import_candidacies_data()` import and preprocess candidacies data,
providing variables related to id of candidacies, abbreviature and names
of them and ballots obtained for each one at each poll station.

``` r
candidacies_data <- import_candidacies_data("congress", 2019, c(4, 11))
#> 🔎 Check if parameters are allowed...
#> 📦 Import candidacies data at poll station level from ...
#> - https://github.com/dadosdelaplace/pollspain-data/blob/main/02-congress/02201904/raw_candidacies_poll_congress_2019_04.rda?raw=true
#> - https://github.com/dadosdelaplace/pollspain-data/blob/main/02-congress/02201911/raw_candidacies_poll_congress_2019_11.rda?raw=true
#> ⏳ Please wait, the volume of data downloaded and the internet connection may take a few seconds
#> ⚠️ A short version was asked. If you require all variables, please run with `short_version = FALSE'
candidacies_data
#> # A tibble: 1,482,129 × 12
#>    id_elec type_elec date_elec  id_INE_poll_station id_INE_mun ccaa  prov  mun  
#>    <glue>  <chr>     <date>     <glue>              <glue>     <chr> <chr> <chr>
#>  1 02-201… congress  2019-04-28 01-04-001-01-001-B  01-04-001  Anda… Alme… Abla 
#>  2 02-201… congress  2019-04-28 01-04-001-01-001-B  01-04-001  Anda… Alme… Abla 
#>  3 02-201… congress  2019-04-28 01-04-002-01-001-A  01-04-002  Anda… Alme… Abru…
#>  4 02-201… congress  2019-04-28 01-04-002-01-001-A  01-04-002  Anda… Alme… Abru…
#>  5 02-201… congress  2019-04-28 01-04-003-01-003-A  01-04-003  Anda… Alme… Adra 
#>  6 02-201… congress  2019-04-28 01-04-003-01-003-B  01-04-003  Anda… Alme… Adra 
#>  7 02-201… congress  2019-04-28 01-04-003-01-003-B  01-04-003  Anda… Alme… Adra 
#>  8 02-201… congress  2019-04-28 01-04-003-01-006-A  01-04-003  Anda… Alme… Adra 
#>  9 02-201… congress  2019-04-28 01-04-003-01-007-U  01-04-003  Anda… Alme… Adra 
#> 10 02-201… congress  2019-04-28 01-04-003-02-002-A  01-04-003  Anda… Alme… Adra 
#> # ℹ 1,482,119 more rows
#> # ℹ 4 more variables: id_candidacies <chr>, abbrev_candidacies <chr>,
#> #   name_candidacies <chr>, ballots <dbl>
```

#### Datos del CERA (pending)

According to the National Statistics Institute (INE):

«The electoral roll contains the registration of those who meet the
requirements to be voters and are not definitively or temporarily
deprived of the right to vote. The electoral roll is composed of:

- The electoral roll of Spanish citizens residing in Spain (CER).

- The electoral roll of Spanish citizens residing abroad (CERA).

The electoral roll of residents in Spain who are nationals of countries
with Agreements for municipal elections (CERE Agreements), and the
electoral roll of citizens of the European Union residing in Spain for
municipal and European Parliament elections (CERE EU).»

The `get_CERA_data()` function returns the data related to the CERA.

``` r
ccaa_CERA_data <- get_CERA_data(election_data, level = "ccaa")
```

### util functions

The functions contained in the `utils.R` script are intended to serve as
helper functions for data preprocessing.

- `type_to_code_election()`: converts from election type (referendum,
  congress, senate, etc.) to the official ministry code.

``` r
type_to_code_election(type_elec = "congress")
#> [1] "02"
```

- `extract_code()`: given a polling station code, it returns the code
  corresponding to the requested level of disaggregation.

``` r
extract_code("01-04-003-01-004-B", level = "mun")
#> [1] "003"
extract_code("01-04-003-01-004-B", level = "mun", full_cod = TRUE)
#> [1] "01-04-003"
```

- `recod_parties()`: recodes party names and their acronyms (pending)
