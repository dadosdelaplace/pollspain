
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pollspain

<!-- badges: start -->
<!-- badges: end -->

## Overview

The goal of pollspain is to …

## Installation

You can install the development version of R package `{pollspain}` from
[GitHub](https://github.com/) through the following code:

``` r
install.packages("devtools") # only if not already installed
devtools::install_github("dadosdelaplace/pollspain")
```

    #> rlang (1.1.0 -> 1.1.1) [CRAN]
    #> xml2  (1.3.3 -> 1.3.4) [CRAN]
    #> vroom (1.6.1 -> 1.6.3) [CRAN]
    #> 
    #> The downloaded binary packages are in
    #>  /var/folders/v8/rwlp504x0_s2b89pn8yrb6km0000gn/T//RtmpvYJY9k/downloaded_packages
    #> ── R CMD build ─────────────────────────────────────────────────────────────────
    #> * checking for file ‘/private/var/folders/v8/rwlp504x0_s2b89pn8yrb6km0000gn/T/RtmpvYJY9k/remotesc9f445fb954/dadosdelaplace-pollspain-5c57c2b/DESCRIPTION’ ... OK
    #> * preparing ‘pollspain’:
    #> * checking DESCRIPTION meta-information ... OK
    #> * checking for LF line-endings in source and make files and shell scripts
    #> * checking for empty or unneeded directories
    #> Removed empty directory ‘pollspain/data/csv/candidacies’
    #> Removed empty directory ‘pollspain/data/csv/candidacies_pollstation’
    #> Removed empty directory ‘pollspain/data/csv/candidates’
    #> Removed empty directory ‘pollspain/data/csv/cod_INE’
    #> Removed empty directory ‘pollspain/data/csv/mun_data’
    #> Removed empty directory ‘pollspain/data/csv/pollstation’
    #> Removed empty directory ‘pollspain/data/csv/summary_data’
    #> Removed empty directory ‘pollspain/data/csv’
    #> * building ‘pollspain_0.1.0.9000.tar.gz’

``` r
library(pollspain)
```

## Usage

### get functions

#### Censo (nivel: municipal)

Variables relativas a población (`pop_res_mun`, `census_INE_mun`,
`census_counting_mun`, `census_CERE_mun`)

``` r
mun_census_data <- get_mun_census_data("congress", 2019, 4)
mun_census_data
#> # A tibble: 16,262 × 20
#>    cod_elec type_elec date_elec  id_INE_mun id_MIR_mun cod_INE_ccaa cod_MIR_ccaa
#>    <chr>    <chr>     <date>     <glue>     <glue>     <chr>        <chr>       
#>  1 02       congress  2019-04-28 01-04-001  01-04-001  01           01          
#>  2 02       congress  2019-04-28 01-04-002  01-04-002  01           01          
#>  3 02       congress  2019-04-28 01-04-003  01-04-003  01           01          
#>  4 02       congress  2019-04-28 01-04-004  01-04-004  01           01          
#>  5 02       congress  2019-04-28 01-04-005  01-04-005  01           01          
#>  6 02       congress  2019-04-28 01-04-006  01-04-006  01           01          
#>  7 02       congress  2019-04-28 01-04-007  01-04-007  01           01          
#>  8 02       congress  2019-04-28 01-04-008  01-04-008  01           01          
#>  9 02       congress  2019-04-28 01-04-009  01-04-009  01           01          
#> 10 02       congress  2019-04-28 01-04-010  01-04-010  01           01          
#> # ℹ 16,252 more rows
#> # ℹ 13 more variables: ccaa <chr>, cod_INE_prov <chr>, prov <chr>,
#> #   cod_INE_mun <chr>, cd_INE_mun <chr>, mun <chr>, cod_mun_jud_district <chr>,
#> #   cod_mun_prov_council <chr>, n_poll_stations <dbl>, pop_res_mun <dbl>,
#> #   census_INE_mun <dbl>, census_counting_mun <dbl>, census_CERE_mun <dbl>
```

#### Datos de mesas electorales (nivel: mesa electoral)

Datos generales de participación, votos en blanco, votos a candidaturas,
etc.

``` r
poll_data <- get_poll_station_data("congress", 2019, c(4, 11))
poll_data
#> # A tibble: 119,697 × 23
#>    id_elec       type_elec date_elec  id_INE_poll_station ccaa      prov   mun  
#>    <glue>        <chr>     <date>     <glue>              <chr>     <chr>  <chr>
#>  1 02-2019-04-28 congress  2019-04-28 01-04-003-01-004-B  Andalucía Almer… Adra 
#>  2 02-2019-04-28 congress  2019-04-28 01-04-003-01-007-U  Andalucía Almer… Adra 
#>  3 02-2019-04-28 congress  2019-04-28 01-04-003-02-001-A  Andalucía Almer… Adra 
#>  4 02-2019-04-28 congress  2019-04-28 01-04-006-01-002-A  Andalucía Almer… Albox
#>  5 02-2019-04-28 congress  2019-04-28 01-04-008-01-001-A  Andalucía Almer… Alcó…
#>  6 02-2019-04-28 congress  2019-04-28 01-04-010-01-001-U  Andalucía Almer… Alha…
#>  7 02-2019-04-28 congress  2019-04-28 01-04-013-02-005-A  Andalucía Almer… Alme…
#>  8 02-2019-04-28 congress  2019-04-28 01-04-013-03-011-A  Andalucía Almer… Alme…
#>  9 02-2019-04-28 congress  2019-04-28 01-04-013-06-013-A  Andalucía Almer… Alme…
#> 10 02-2019-04-28 congress  2019-04-28 01-04-013-06-016-B  Andalucía Almer… Alme…
#> # ℹ 119,687 more rows
#> # ℹ 16 more variables: census_counting <dbl>, ballots_1 <dbl>, turnout_1 <dbl>,
#> #   ballots_2 <dbl>, turnout_2 <dbl>, blank_ballots <dbl>,
#> #   invalid_ballots <dbl>, party_ballots <dbl>, valid_ballots <dbl>,
#> #   total_ballots <dbl>, turnout <dbl>, porc_valid <dbl>, porc_invalid <dbl>,
#> #   porc_parties <dbl>, porc_blank <dbl>, pop_res_mun <dbl>
```

#### Datos de candidatos/as (nivel: circunscripción electoral)

Datos de los/as candidatos que forman las listas (por circunscripción
provincial en las generales)

``` r
candidates_data <- get_candidates_data("congress", 2019, c(4, 11))
candidates_data 
#> # A tibble: 11,182 × 15
#>    cod_elec type_elec date_elec   turn cod_INE_prov cod_mun_district cod_INE_mun
#>    <chr>    <chr>     <date>     <dbl> <chr>        <chr>            <chr>      
#>  1 02       congress  2019-04-28     1 11           <NA>             <NA>       
#>  2 02       congress  2019-04-28     1 11           <NA>             <NA>       
#>  3 02       congress  2019-04-28     1 11           <NA>             <NA>       
#>  4 02       congress  2019-04-28     1 11           <NA>             <NA>       
#>  5 02       congress  2019-04-28     1 11           <NA>             <NA>       
#>  6 02       congress  2019-04-28     1 11           <NA>             <NA>       
#>  7 02       congress  2019-04-28     1 11           <NA>             <NA>       
#>  8 02       congress  2019-04-28     1 11           <NA>             <NA>       
#>  9 02       congress  2019-04-28     1 11           <NA>             <NA>       
#> 10 02       congress  2019-04-28     1 11           <NA>             <NA>       
#> # ℹ 11,172 more rows
#> # ℹ 8 more variables: id_candidacies <chr>, order <dbl>, holder <lgl>,
#> #   name <chr>, surname <chr>, sex <chr>, id_card <chr>, elected <lgl>
```

#### Datos de candidaturas (nivel: mesa electoral)

Datos de las candidaturas por mesa electoral, con los votos obtenidos en
cada mesa para cada candidatura, así como los electos por provincia
obtenidos

``` r
candidacies_data <- get_candidacies_data("congress", 2019, c(4, 11))
candidacies_data
#> # A tibble: 1,482,766 × 12
#>    type_elec date_elec  id_INE_poll_station ccaa      prov  mun   id_candidacies
#>    <chr>     <date>     <glue>              <chr>     <chr> <chr> <chr>         
#>  1 congress  2019-04-28 01-04-001-01-001-B  Andalucía Alme… Abla  000077        
#>  2 congress  2019-04-28 01-04-001-01-001-B  Andalucía Alme… Abla  000117        
#>  3 congress  2019-04-28 01-04-002-01-001-A  Andalucía Alme… Abru… 000028        
#>  4 congress  2019-04-28 01-04-002-01-001-A  Andalucía Alme… Abru… 000054        
#>  5 congress  2019-04-28 01-04-003-01-003-A  Andalucía Alme… Adra  000022        
#>  6 congress  2019-04-28 01-04-003-01-003-B  Andalucía Alme… Adra  000022        
#>  7 congress  2019-04-28 01-04-003-01-003-B  Andalucía Alme… Adra  000077        
#>  8 congress  2019-04-28 01-04-003-01-006-A  Andalucía Alme… Adra  000022        
#>  9 congress  2019-04-28 01-04-003-01-007-U  Andalucía Alme… Adra  000117        
#> 10 congress  2019-04-28 01-04-003-02-002-A  Andalucía Alme… Adra  000104        
#> # ℹ 1,482,756 more rows
#> # ℹ 5 more variables: id_candidacies_prov <chr>, abbrev_candidacies <chr>,
#> #   name_candidacies <chr>, ballots <dbl>, elected_by_prov <dbl>
```

#### Datos del CERA

Según el INE:

«El censo electoral contiene la inscripción de quienes reúnen los
requisitos para ser elector y no se hallen privados, definitiva o
temporalmente, del derecho de sufragio. El censo electoral está
compuesto por:

- El censo electoral de españoles residentes en España (CER).
- El censo electoral de españoles residentes-ausentes que viven en el
  extranjero (CERA).

El censo electoral de residentes en España que sean nacionales de países
con Acuerdos para las elecciones municipales (CERE Acuerdos), y el censo
electoral de ciudadanos de la Unión Europea residentes en España para
las elecciones municipales y al Parlamento Europeo (CERE UE).»

La función `get_CERA_data()` nos devuelve los datos relativos al CERA

``` r
ccaa_CERA_data <- get_CERA_data(election_data, level = "ccaa")
```

#### Datos electorales agregados

La función `get_elections_data()` nos permite obtener los datos
electorales de las elecciones pedidas y con el nivel (`level`) de
agregación que queramos, pudiendo ser `all`, `ccaa`, `prov`, `mun`,
`mun_district`, `sec` (sección censal) y `poll_station` (a nivel de mesa
electoral)

``` r
prov_data <- get_elections_data("congress", year = 2019, month = c(4, 11), level = "prov")
#> Warning in get_elections_data("congress", year = 2019, month = c(4, 11), :
#> Since include_candidacies = FALSE, aggregating by parties has not been
#> implemented
prov_data
#> # A tibble: 104 × 27
#>    id_elec    type_elec date_elec  pop_res cod_INE_ccaa cod_INE_prov ccaa  prov 
#>    <glue>     <chr>     <date>       <dbl> <chr>        <chr>        <chr> <chr>
#>  1 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#>  2 02-2019-0… congress  2019-04-28 1238714 01           11           Anda… Cádiz
#>  3 02-2019-0… congress  2019-04-28  785240 01           14           Anda… Córd…
#>  4 02-2019-0… congress  2019-04-28  912075 01           18           Anda… Gran…
#>  5 02-2019-0… congress  2019-04-28  519932 01           21           Anda… Huel…
#>  6 02-2019-0… congress  2019-04-28  638099 01           23           Anda… Jaén 
#>  7 02-2019-0… congress  2019-04-28 1641121 01           29           Anda… Mála…
#>  8 02-2019-0… congress  2019-04-28 1939887 01           41           Anda… Sevi…
#>  9 02-2019-0… congress  2019-04-28  219345 02           22           Arag… Hues…
#> 10 02-2019-0… congress  2019-04-28  134572 02           44           Arag… Teru…
#> # ℹ 94 more rows
#> # ℹ 19 more variables: n_poll_stations <int>, census_counting <dbl>,
#> #   ballots_1 <dbl>, ballots_2 <dbl>, blank_ballots <dbl>,
#> #   invalid_ballots <dbl>, party_ballots <dbl>, valid_ballots <dbl>,
#> #   total_ballots <dbl>, turnout_1 <dbl>, turnout_2 <dbl>, turnout <dbl>,
#> #   porc_valid <dbl>, porc_invalid <dbl>, porc_parties <dbl>, porc_blank <dbl>,
#> #   census_cera <dbl>, total_ballots_cera <dbl>, turnout_cera <dbl>
```

Esta función combina `get_poll_station_data()` (el dato en bruto por
mesa electoral) con `get_CERA_data()` y `aggregate_election_data()`, que
nos proporciona la agregación pedida. El código anterior es equivalente
a:

``` r
# Raw data at poll station level
election_data <-
      get_poll_station_data("congress", year = 2019, month = c(4, 11))

# and then aggregate at provided level
prov_data <-
  election_data |>
  aggregate_election_data(level = "prov")
prov_data
#> # A tibble: 104 × 27
#>    id_elec    type_elec date_elec  pop_res cod_INE_ccaa cod_INE_prov ccaa  prov 
#>    <glue>     <chr>     <date>       <dbl> <chr>        <chr>        <chr> <chr>
#>  1 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#>  2 02-2019-0… congress  2019-04-28 1238714 01           11           Anda… Cádiz
#>  3 02-2019-0… congress  2019-04-28  785240 01           14           Anda… Córd…
#>  4 02-2019-0… congress  2019-04-28  912075 01           18           Anda… Gran…
#>  5 02-2019-0… congress  2019-04-28  519932 01           21           Anda… Huel…
#>  6 02-2019-0… congress  2019-04-28  638099 01           23           Anda… Jaén 
#>  7 02-2019-0… congress  2019-04-28 1641121 01           29           Anda… Mála…
#>  8 02-2019-0… congress  2019-04-28 1939887 01           41           Anda… Sevi…
#>  9 02-2019-0… congress  2019-04-28  219345 02           22           Arag… Hues…
#> 10 02-2019-0… congress  2019-04-28  134572 02           44           Arag… Teru…
#> # ℹ 94 more rows
#> # ℹ 19 more variables: n_poll_stations <int>, census_counting <dbl>,
#> #   ballots_1 <dbl>, ballots_2 <dbl>, blank_ballots <dbl>,
#> #   invalid_ballots <dbl>, party_ballots <dbl>, valid_ballots <dbl>,
#> #   total_ballots <dbl>, turnout_1 <dbl>, turnout_2 <dbl>, turnout <dbl>,
#> #   porc_valid <dbl>, porc_invalid <dbl>, porc_parties <dbl>, porc_blank <dbl>,
#> #   census_cera <dbl>, total_ballots_cera <dbl>, turnout_cera <dbl>
```
