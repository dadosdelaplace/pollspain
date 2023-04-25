
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

    #> Downloading GitHub repo dadosdelaplace/pollspain@HEAD
    #> 
    #> ── R CMD build ─────────────────────────────────────────────────────────────────
    #> * checking for file ‘/private/var/folders/v8/rwlp504x0_s2b89pn8yrb6km0000gn/T/RtmpPJTghs/remotes456f4a0661e0/dadosdelaplace-pollspain-a60659c/DESCRIPTION’ ... OK
    #> * preparing ‘pollspain’:
    #> * checking DESCRIPTION meta-information ... OK
    #> * checking for LF line-endings in source and make files and shell scripts
    #> * checking for empty or unneeded directories
    #> * building ‘pollspain_0.1.0.9000.tar.gz’
    #> Installing package into '/private/var/folders/v8/rwlp504x0_s2b89pn8yrb6km0000gn/T/RtmpfS6LqJ/temp_libpath42e458e967b'
    #> (as 'lib' is unspecified)

``` r
library(pollspain)
```

## Usage

### get functions

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
