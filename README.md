
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

    #> 
    #> ── R CMD build ─────────────────────────────────────────────────────────────────
    #> * checking for file ‘/private/var/folders/v8/rwlp504x0_s2b89pn8yrb6km0000gn/T/RtmpIqUQ2y/remotes65f6213cc128/dadosdelaplace-pollspain-a60659c/DESCRIPTION’ ... OK
    #> * preparing ‘pollspain’:
    #> * checking DESCRIPTION meta-information ... OK
    #> * checking for LF line-endings in source and make files and shell scripts
    #> * checking for empty or unneeded directories
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
#> # A tibble: 8,131 × 19
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
#> # ℹ 8,121 more rows
#> # ℹ 12 more variables: ccaa <chr>, cod_INE_prov <chr>, prov <chr>,
#> #   cod_INE_mun <chr>, cd_INE_mun <chr>, mun <chr>, cod_mun_jud_district <chr>,
#> #   cod_mun_prov_council <chr>, pop_res_mun <dbl>, census_INE_mun <dbl>,
#> #   census_counting_mun <dbl>, census_CERE_mun <dbl>
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
#> Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
#> dplyr 1.1.0.
#> ℹ Please use `reframe()` instead.
#> ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
#>   always returns an ungrouped data frame and adjust accordingly.
#> ℹ The deprecated feature was likely used in the pollspain package.
#>   Please report the issue to the authors.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
candidates_data 
#> # A tibble: 8,585 × 15
#>    cod_elec type_elec date_elec   turn cod_INE_prov cod_mun_district cod_INE_mun
#>    <chr>    <chr>     <date>     <dbl> <chr>        <lgl>            <lgl>      
#>  1 02       congress  2019-04-28     1 11           NA               NA         
#>  2 02       congress  2019-04-28     1 11           NA               NA         
#>  3 02       congress  2019-04-28     1 11           NA               NA         
#>  4 02       congress  2019-04-28     1 11           NA               NA         
#>  5 02       congress  2019-04-28     1 11           NA               NA         
#>  6 02       congress  2019-04-28     1 11           NA               NA         
#>  7 02       congress  2019-04-28     1 11           NA               NA         
#>  8 02       congress  2019-04-28     1 11           NA               NA         
#>  9 02       congress  2019-04-28     1 11           NA               NA         
#> 10 02       congress  2019-04-28     1 11           NA               NA         
#> # ℹ 8,575 more rows
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
