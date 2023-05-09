
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
    #> httr  (1.4.5 -> 1.4.6) [CRAN]
    #> vroom (1.6.1 -> 1.6.3) [CRAN]
    #> 
    #>   There is a binary version available but the source version is later:
    #>      binary source needs_compilation
    #> httr  1.4.5  1.4.6             FALSE
    #> 
    #> 
    #> The downloaded binary packages are in
    #>  /var/folders/v8/rwlp504x0_s2b89pn8yrb6km0000gn/T//RtmpPgF5Rp/downloaded_packages
    #> ── R CMD build ─────────────────────────────────────────────────────────────────
    #> * checking for file ‘/private/var/folders/v8/rwlp504x0_s2b89pn8yrb6km0000gn/T/RtmpPgF5Rp/remotes6b3e4a4bfd32/dadosdelaplace-pollspain-ec39fb7/DESCRIPTION’ ... OK
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
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.2     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.0
#> ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
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
#> # A tibble: 1,482,129 × 13
#>    id_elec       type_elec date_elec  id_INE_poll_station ccaa      prov   mun  
#>    <glue>        <chr>     <date>     <glue>              <chr>     <chr>  <chr>
#>  1 02-2019-04-28 congress  2019-04-28 01-04-001-01-001-B  Andalucía Almer… Abla 
#>  2 02-2019-04-28 congress  2019-04-28 01-04-001-01-001-B  Andalucía Almer… Abla 
#>  3 02-2019-04-28 congress  2019-04-28 01-04-002-01-001-A  Andalucía Almer… Abru…
#>  4 02-2019-04-28 congress  2019-04-28 01-04-002-01-001-A  Andalucía Almer… Abru…
#>  5 02-2019-04-28 congress  2019-04-28 01-04-003-01-003-A  Andalucía Almer… Adra 
#>  6 02-2019-04-28 congress  2019-04-28 01-04-003-01-003-B  Andalucía Almer… Adra 
#>  7 02-2019-04-28 congress  2019-04-28 01-04-003-01-003-B  Andalucía Almer… Adra 
#>  8 02-2019-04-28 congress  2019-04-28 01-04-003-01-006-A  Andalucía Almer… Adra 
#>  9 02-2019-04-28 congress  2019-04-28 01-04-003-01-007-U  Andalucía Almer… Adra 
#> 10 02-2019-04-28 congress  2019-04-28 01-04-003-02-002-A  Andalucía Almer… Adra 
#> # ℹ 1,482,119 more rows
#> # ℹ 6 more variables: id_candidacies <chr>, id_candidacies_prov <chr>,
#> #   abbrev_candidacies <chr>, name_candidacies <chr>, ballots <dbl>,
#> #   elected_by_prov <dbl>
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
#> 🔎 Check if parameters are allowed...
#>    🔔 Since include_candidacies = FALSE, aggregating by parties has not been implemented
#> 📦 Get poll station data...
#>    - Download poll station data...
#>    - Aggregating election data at prov level...
#> ✅ Last summaries and tasks...
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

#### Datos de elecciones

Función principal, combinando la extracción y la agregación

``` r
national_data <-
  get_elections_data("congress", 2019, c(4, 11), 
                     include_candidacies = TRUE, level =  "all")
#> 🔎 Check if parameters are allowed...
#> 📦 Get poll station data...
#>    - Download poll station data...
#>    - Aggregating election data at national level...
#> 📦 Get candidacies (parties) data...
#>    - Download candidacies (parties) data... (please wait, intensive task)
#>    - Aggregating candidacies data at national level...
#> 🖇 Join information...
#> ✅ Last summaries and tasks...
national_data |> arrange(desc(ballots))
#> # A tibble: 134 × 34
#>    id_elec       type_elec date_elec   pop_res id_candidacies abbrev_candidacies
#>    <glue>        <chr>     <date>        <dbl> <chr>          <chr>             
#>  1 02-2019-04-28 congress  2019-04-28 46722980 000096         PSOE              
#>  2 02-2019-11-10 congress  2019-11-10 46722980 000094         PSOE              
#>  3 02-2019-11-10 congress  2019-11-10 46722980 000083         PP                
#>  4 02-2019-04-28 congress  2019-04-28 46722980 000083         PP                
#>  5 02-2019-04-28 congress  2019-04-28 46722980 000022         CS                
#>  6 02-2019-11-10 congress  2019-11-10 46722980 000116         VOX               
#>  7 02-2019-04-28 congress  2019-04-28 46722980 000077         UP                
#>  8 02-2019-04-28 congress  2019-04-28 46722980 000117         VOX               
#>  9 02-2019-11-10 congress  2019-11-10 46722980 000078         UP                
#> 10 02-2019-11-10 congress  2019-11-10 46722980 000018         CS                
#> # ℹ 124 more rows
#> # ℹ 28 more variables: name_candidacies <chr>, ballots <dbl>, elected <dbl>,
#> #   ballots_by_elec <dbl>, porc_candidacies_parties <dbl>,
#> #   porc_candidacies_valid <dbl>, porc_candidacies_census <dbl>,
#> #   porc_elected <dbl>, anomaly_ballots_elected <dbl>, n_poll_stations <int>,
#> #   census_counting <dbl>, ballots_1 <dbl>, ballots_2 <dbl>,
#> #   blank_ballots <dbl>, invalid_ballots <dbl>, party_ballots <dbl>, …
```

``` r
ccaa_data <-
  get_elections_data("congress", 2019, c(4, 11), 
                     include_candidacies = TRUE, level = "ccaa")
#> 🔎 Check if parameters are allowed...
#> 📦 Get poll station data...
#>    - Download poll station data...
#>    - Aggregating election data at ccaa level...
#> 📦 Get candidacies (parties) data...
#>    - Download candidacies (parties) data... (please wait, intensive task)
#>    - Aggregating candidacies data at ccaa level...
#> 🖇 Join information...
#> ✅ Last summaries and tasks...
ccaa_data
#> # A tibble: 503 × 36
#>    id_elec       type_elec date_elec  pop_res cod_INE_ccaa ccaa   id_candidacies
#>    <glue>        <chr>     <date>       <dbl> <chr>        <chr>  <chr>         
#>  1 02-2019-04-28 congress  2019-04-28 8384408 01           Andal… 000077        
#>  2 02-2019-04-28 congress  2019-04-28 8384408 01           Andal… 000117        
#>  3 02-2019-04-28 congress  2019-04-28 8384408 01           Andal… 000028        
#>  4 02-2019-04-28 congress  2019-04-28 8384408 01           Andal… 000054        
#>  5 02-2019-04-28 congress  2019-04-28 8384408 01           Andal… 000022        
#>  6 02-2019-04-28 congress  2019-04-28 8384408 01           Andal… 000104        
#>  7 02-2019-04-28 congress  2019-04-28 8384408 01           Andal… 000058        
#>  8 02-2019-04-28 congress  2019-04-28 8384408 01           Andal… 000096        
#>  9 02-2019-04-28 congress  2019-04-28 8384408 01           Andal… 000100        
#> 10 02-2019-04-28 congress  2019-04-28 8384408 01           Andal… 000083        
#> # ℹ 493 more rows
#> # ℹ 29 more variables: abbrev_candidacies <chr>, name_candidacies <chr>,
#> #   ballots <dbl>, elected <dbl>, ballots_by_elec <dbl>,
#> #   porc_candidacies_parties <dbl>, porc_candidacies_valid <dbl>,
#> #   porc_candidacies_census <dbl>, porc_elected <dbl>,
#> #   anomaly_ballots_elected <dbl>, n_poll_stations <int>,
#> #   census_counting <dbl>, ballots_1 <dbl>, ballots_2 <dbl>, …

prov_data <-
  get_elections_data("congress", 2019, c(4, 11), 
                     include_candidacies = TRUE, level = "prov")
#> 🔎 Check if parameters are allowed...
#> 📦 Get poll station data...
#>    - Download poll station data...
#>    - Aggregating election data at prov level...
#> 📦 Get candidacies (parties) data...
#>    - Download candidacies (parties) data... (please wait, intensive task)
#>    - Aggregating candidacies data at prov level...
#> 🖇 Join information...
#> ✅ Last summaries and tasks...
prov_data
#> # A tibble: 1,210 × 38
#>    id_elec    type_elec date_elec  pop_res cod_INE_ccaa cod_INE_prov ccaa  prov 
#>    <glue>     <chr>     <date>       <dbl> <chr>        <chr>        <chr> <chr>
#>  1 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#>  2 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#>  3 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#>  4 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#>  5 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#>  6 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#>  7 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#>  8 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#>  9 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#> 10 02-2019-0… congress  2019-04-28  709340 01           04           Anda… Alme…
#> # ℹ 1,200 more rows
#> # ℹ 30 more variables: id_candidacies <chr>, abbrev_candidacies <chr>,
#> #   name_candidacies <chr>, ballots <dbl>, elected <dbl>,
#> #   ballots_by_elec <dbl>, porc_candidacies_parties <dbl>,
#> #   porc_candidacies_valid <dbl>, porc_candidacies_census <dbl>,
#> #   porc_elected <dbl>, anomaly_ballots_elected <dbl>, n_poll_stations <int>,
#> #   census_counting <dbl>, ballots_1 <dbl>, ballots_2 <dbl>, …
```

Es lo mismo extraer a nivel municipio que a un nivel más bajo y luego
agrupar y sumarizar.

``` r
mun_data <-
  get_elections_data("congress", 2019, c(4, 11), 
                     include_candidacies = TRUE, level = "mun")
#> 🔎 Check if parameters are allowed...
#> 📦 Get poll station data...
#>    - Download poll station data...
#>    - Aggregating election data at mun level...
#> 📦 Get candidacies (parties) data...
#>    - Download candidacies (parties) data... (please wait, intensive task)
#>    - Aggregating candidacies data at mun level...
#> 🖇 Join information...
#> ✅ Last summaries and tasks...
mun_data |> filter(date_elec == "2019-04-28" & mun == "Dos Hermanas")
#> # A tibble: 13 × 40
#>    id_elec    type_elec date_elec  pop_res cod_INE_ccaa cod_INE_prov cod_INE_mun
#>    <glue>     <chr>     <date>       <dbl> <chr>        <chr>        <chr>      
#>  1 02-2019-0… congress  2019-04-28  133168 01           41           038        
#>  2 02-2019-0… congress  2019-04-28  133168 01           41           038        
#>  3 02-2019-0… congress  2019-04-28  133168 01           41           038        
#>  4 02-2019-0… congress  2019-04-28  133168 01           41           038        
#>  5 02-2019-0… congress  2019-04-28  133168 01           41           038        
#>  6 02-2019-0… congress  2019-04-28  133168 01           41           038        
#>  7 02-2019-0… congress  2019-04-28  133168 01           41           038        
#>  8 02-2019-0… congress  2019-04-28  133168 01           41           038        
#>  9 02-2019-0… congress  2019-04-28  133168 01           41           038        
#> 10 02-2019-0… congress  2019-04-28  133168 01           41           038        
#> 11 02-2019-0… congress  2019-04-28  133168 01           41           038        
#> 12 02-2019-0… congress  2019-04-28  133168 01           41           038        
#> 13 02-2019-0… congress  2019-04-28  133168 01           41           038        
#> # ℹ 33 more variables: ccaa <chr>, prov <chr>, mun <chr>, id_candidacies <chr>,
#> #   abbrev_candidacies <chr>, name_candidacies <chr>, ballots <dbl>,
#> #   elected <dbl>, ballots_by_elec <dbl>, porc_candidacies_parties <dbl>,
#> #   porc_candidacies_valid <dbl>, porc_candidacies_census <dbl>,
#> #   porc_elected <dbl>, anomaly_ballots_elected <dbl>, n_poll_stations <dbl>,
#> #   census_counting <dbl>, ballots_1 <dbl>, ballots_2 <dbl>,
#> #   blank_ballots <dbl>, invalid_ballots <dbl>, party_ballots <dbl>, …


mun_district_data <- get_elections_data("congress", 2019, c(4, 11),
                                        include_candidacies = TRUE,
                                        level = "mun_district")
#> 🔎 Check if parameters are allowed...
#> 📦 Get poll station data...
#>    - Download poll station data...
#>    - Aggregating election data at mun_district level...
#> 📦 Get candidacies (parties) data...
#>    - Download candidacies (parties) data... (please wait, intensive task)
#>    - Aggregating candidacies data at mun_district level...
#> 🖇 Join information...
#> ✅ Last summaries and tasks...
mun_district_data |>
  group_by(id_elec, type_elec, date_elec, cod_INE_mun, mun, id_candidacies) |>
  summarise(sum(ballots)) |>
  ungroup() |> 
  filter(date_elec == "2019-04-28" & mun == "Dos Hermanas")
#> `summarise()` has grouped output by 'id_elec', 'type_elec', 'date_elec',
#> 'cod_INE_mun', 'mun'. You can override using the `.groups` argument.
#> # A tibble: 13 × 7
#>    id_elec  type_elec date_elec  cod_INE_mun mun   id_candidacies `sum(ballots)`
#>    <glue>   <chr>     <date>     <chr>       <chr> <chr>                   <dbl>
#>  1 02-2019… congress  2019-04-28 038         Dos … 000011                    195
#>  2 02-2019… congress  2019-04-28 038         Dos … 000022                  13944
#>  3 02-2019… congress  2019-04-28 038         Dos … 000054                   1436
#>  4 02-2019… congress  2019-04-28 038         Dos … 000056                    279
#>  5 02-2019… congress  2019-04-28 038         Dos … 000057                    118
#>  6 02-2019… congress  2019-04-28 038         Dos … 000058                     73
#>  7 02-2019… congress  2019-04-28 038         Dos … 000066                     69
#>  8 02-2019… congress  2019-04-28 038         Dos … 000077                  13183
#>  9 02-2019… congress  2019-04-28 038         Dos … 000083                   7915
#> 10 02-2019… congress  2019-04-28 038         Dos … 000096                  26918
#> 11 02-2019… congress  2019-04-28 038         Dos … 000100                     95
#> 12 02-2019… congress  2019-04-28 038         Dos … 000104                    149
#> 13 02-2019… congress  2019-04-28 038         Dos … 000117                  10232
```
