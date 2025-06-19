
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pollspain <img src="man/figures/pollspain_sticker.png" align="right" width="120"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project-Status:Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub
release](https://img.shields.io/github/v/release/dadosdelaplace/pollspain)](https://github.com/dadosdelaplace/pollspain/releases)
[![R-CMD-check](https://github.com/dadosdelaplace/pollspain/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dadosdelaplace/pollspain/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The main objective of the R package `{pollspain}` is to provide social
scientists, political analysts and citizens with easy and
straightforward <span class="hl">**access to electoral data from
Spain**</span>. This includes both aggregated election results extracted
from polling stations and survey data (including housing effects). It
also offers tools for seat allocation, vote simulation, and dataviz. The
package is designed under <span class="hl">**tidyverse-style**</span>
specially tailored for beginners.

## Installation

You can <span class="hl">**install the development version**</span> from
[GitHub](https://github.com/) with

``` r
# install.packages("devtools") # only if not already installed
devtools::install_github("dadosdelaplace/pollspain")
library(pollspain) # after installing
```

An **internet connection** is just required for installing. Data is
stored in the accompanying `pollspaindata` package. See more at
<https://github.com/dadosdelaplace/pollspaindata>

## Usage

### Election summaries

Using `summary_election_data(type_elec = ..., year = ..., level = ...)`
with

- a vector of **election dates** (e.g., `year = 2023` or
  `date = "2023-07-24"`)
- a vector of **election types** (currently, it only works properly for
  `type_elec = "congress"`)
- a geographic **level for aggregation** (e.g., `level = "ccaa"` or
  `level = "prov"`)

returns a <span class="hl">**summary of election results aggregated at
the administrative level**</span>. This includes both general data
(blank votes, turnout, etc) and ballots by each candidacy. The available
<span class="hl">**levels**</span> (`level`) are: `"all"`, `"ccaa"`
(autonomous communities), `"prov"` (province), `"mun"` (municipality),
`"mun_district"` (district), `"sec"` (census tract), and
`"poll_station"`.

``` r
# Summary election data at ccaa level for both elections in 2023 and 2016
summary_data_all <-
  summary_election_data(type_elec = "congress", year = 2023, date = "2016-06-26")
summary_data_all
```

    #> # A tibble: 6 × 13
    #>   id_elec       blank_ballots invalid_ballots party_ballots valid_ballots
    #>   <chr>                 <dbl>           <dbl>         <dbl>         <dbl>
    #> 1 02-2016-06-26        179081          225504      23874674      24053755
    #> 2 02-2016-06-26        179081          225504      23874674      24053755
    #> 3 02-2016-06-26        179081          225504      23874674      24053755
    #> 4 02-2023-07-24        200682          264382      24487589      24688271
    #> 5 02-2023-07-24        200682          264382      24487589      24688271
    #> 6 02-2023-07-24        200682          264382      24487589      24688271
    #>   total_ballots id_candidacies abbrev_candidacies
    #>           <dbl> <chr>          <chr>             
    #> 1      24279259 000068         PP                
    #> 2      24279259 000077         PSOE              
    #> 3      24279259 000059         PODEMOS           
    #> 4      24952653 000005         PP                
    #> 5      24952653 000002         PSOE              
    #> 6      24952653 000006         VOX               
    #>   name_candidacies                  ballots porc_candidacies_parties
    #>   <chr>                               <dbl>                    <dbl>
    #> 1 PARTIDO POPULAR                   7941236                     33.3
    #> 2 PARTIDO SOCIALISTA OBRERO ESPANOL 5443846                     22.8
    #> 3 PODEMOS                           3227123                     13.5
    #> 4 PARTIDO POPULAR                   8161117                     33.3
    #> 5 PARTIDO SOCIALISTA OBRERO ESPANOL 7821777                     31.9
    #> 6 VOX                               3057068                     12.5
    #>   porc_candidacies_valid porc_candidacies_census
    #>                    <dbl>                   <dbl>
    #> 1                   33.0                   23.0 
    #> 2                   22.6                   15.7 
    #> 3                   13.4                    9.33
    #> 4                   33.1                   23.2 
    #> 5                   31.7                   22.3 
    #> 6                   12.4                    8.70

See [**some use cases and
tutorials**](https://javieralvarezliebana.es/pollspain/#tutorials).

<details>

<summary>

⚠️ About data
</summary>

Details about data can be found in
<https://javieralvarezliebana.es/pollspain/articles/about-data.html>. It
is important to note that in some polling stations, the
<span class="hl">**data from the Ministry of the Interior is corrupted
or inconsistent**</span>, as the vote counts by party do not match the
total vote counts per polling station when aggregated. In such cases,
the data has been manually summarized using the disaggregated results by
polling station and party. The <span class="hl">**files from the 2015
general election are particularly problematic**</span>, as they
sometimes include CERA votes linked to party id’s for which no further
information is available in the Ministry’s own files. The authors of the
package have contacted the Ministry to report these issues but have not
received any response.

</details>

<details>

<summary>

⚠️ About municipalities
</summary>

The <span class="hl">**municipality data (names and codes) were
extracted from the version published by the National Statistics
Institute (INE)**</span> on February 6, 2025\*\*. Over the years,
various municipal mergers have taken place in Spain, which means that
not all elections feature the same set of municipalities or the same
identifying codes. In order to unify and standardize the results
provided to users, all output tables refer to the most recent
municipality recoding by the Spanish National Statistics Institute
(INE). The helper function `recod_mun()` transparently returns the
updated code based on that recoding, reassigning codes for merged
municipalities accordingly.

Data extracted from
<https://www.ine.es/daco/daco42/codmun/codmun20/20codmun.xlsx>.
</details>

<details>

<summary>

⚠️ About CERA
</summary>

According to the National Statistics Institute (INE) «the electoral roll
contains the registration of those who meet the requirements to be
voters and are not definitively or temporarily deprived of the right to
vote. The electoral roll is composed of:

- The electoral roll of Spanish citizens residing in Spain (CER).
- The electoral roll of Spanish citizens residing abroad (CERA).

The electoral roll of residents in Spain who are nationals of countries
with Agreements for municipal elections (CERE Agreements), and the
electoral roll of citizens of the European Union residing in Spain for
municipal and European Parliament elections (CERE EU)».

<span class="hl">**CERA-related data is provided at the provincial
constituency level**</span>. These votes are aggregated with the rest
when `level = "all"`, `level = "ccaa"`, or `level = "prov"`. At any
lower level of aggregation, 52 additional rows—one for each province—are
included (for example, when aggregating data at the municipal level, the
result includes as many rows as there are municipalities plus 52 «CERA
municipalities»).

</details>

### Seat allocation

Using
`seat_allocation(candidacies = ..., ballots = ..., blank_ballots = ..., n_seats = ..., method = ..., threshold = ..., short_version = ...)`
with

- a vector of **abbreviations or id** for candidacies (e.g.,
  `candidacies = c("PP", "PSOE", "VOX")`) and vector of their
  **ballots** (e.g., `ballots = c(250, 400, 150)`)
- a number of **blank ballots** (e.g., `blank_ballots = 100`)
- a number of **seats to be allocated** (e.g., `n_seats = 5`)
- a set of **apportionment methods** (e.g.,
  `method = c("hondt", "webster")`)
- a numerical threshold (e.g., `threshold = 0.03`)

returns a <span class="hl">**tibble with the number of seats for each
candidacy and each apportionment method**</span>, or event the
intermediate quotients of the allocation process for divisor methods.

``` r
# Summary election data, aggregating candidacies ballots at prov level,
# and then allocation of seats for Sevilla with dhondt and x... method
summary_election_data("congress", year = 2023, level = "prov",
                      verbose = FALSE) |> 
  filter(prov == "Sevilla") |> 
  reframe(seat_allocation(candidacies = abbrev_candidacies,
                          ballots = ballots, blank_ballots = blank_ballots,
                          method = c("hondt"),
                          n_seats = 12, threshold = 0.03))
#> # A tibble: 11 × 3
#>    candidacies   method seats
#>    <chr>         <chr>  <dbl>
#>  1 PSOE          hondt      5
#>  2 PP            hondt      4
#>  3 SUMAR         hondt      2
#>  4 VOX           hondt      1
#>  5 PUMJ          hondt      0
#>  6 PACMA         hondt      0
#>  7 FO            hondt      0
#>  8 PCTE          hondt      0
#>  9 FE-JONS       hondt      0
#> 10 CJ            hondt      0
#> 11 RECORTES-CERO hondt      0
```

### Surveys summaries

### Data viz

<!--
* barras ordenadas a más a menos (con colores)
* ggparlament
* encuestas + promedio
* barras con resultados + encuestas encima
* barras con % de voto vs %escaños?
* mapa
* ¿algún lollipop para mostrar housing efects? con flechas y eso.
-->

## About data

This package uses data collected from the following sources:

- Spanish electoral data downloaded from [**repository of the Spanish
  Ministry of the
  Interior**](https://infoelectoral.interior.gob.es/es/elecciones-celebradas/area-de-descargas/).

- Worldwide electoral data downloaded from [**Comparative Study of
  Electoral Systems (CSES)**](https://cses.org/data-download/) and
  [**Election Data Archive (ICPSR)**](https://electiondataarchive.org/)

- Survey data download from Wikipedia links
  ([**example**](https://en.wikipedia.org/wiki/Opinion_polling_for_the_2023_Spanish_general_election)).

- Seat allocation methods checked in the [**Electoral System Design
  Database (International
  IDEA)**](https://www.idea.int/data-tools/data/electoral-system-design)

See more details about data structure at
<https://javieralvarezliebana.es/pollspain/articles/about-data.html>

## Tutorials

- [**How to use utils
  functions?**](https://javieralvarezliebana.es/pollspain/articles/utils.html)

- [**Import raw election and survey files**](...)

- [**Getting election summaries**](...)

- [**Creating electoral maps (pollspain + mapSpain)**](...)

- [**Linking income data to electoral results (pollspain +
  ineAtlas)**](...)

- [**About seat allocation methods**](...)

- [**Getting survey summaries**](...)

- [**About survey house effects**](...)

## Other functions

`{pollspain}` also provides <span class="hl">**more advanced users with
useful functions**</span> to preprocess and analyze electoral data—even
their own data, as long as it is provided in a proper format.

- <span class="hl">**Utils**</span>: functions contained in the
  `utils.R` script are intended to serve as helper functions for data
  preprocessing. See [**more examples and use
  cases**](https://javieralvarezliebana.es/pollspain/articles/utils.html)
  about how to use them.

``` r
type_to_code_election(type_elec = "congress")
#> [1] "02"
extract_code("01-04-003-01-004-B", level = "mun")
#> [1] "003"
extract_code("01-04-003-01-004-B", level = "mun", full_cod = TRUE)
#> [1] "01-04-003"
```

- <span class="hl">**Import raw data**</span>: functions starting with
  `import_..._data()` (code can be found in `import_elections_data.R`
  file) are aimed at importing and preprocessing as raw as possible the
  `.DAT` election files from the files available in the [pollspaindata
  package](https://github.com/dadosdelaplace/pollspaindata). See [**more
  examples about how to use them**](...).

## Contributing

Any contribution is warmly welcome, whether as a developer or a beta
tester of the package. Please feel free to propose any suggestions by
[**opening an issue for
discussion**](https://github.com/dadosdelaplace/pollspain/issues).

## References

This package has been designed based on the <span class="hl">**following
resources and references**</span>

- ACE: The Electoral Knowledge Network. <https://aceproject.org/>
- Albuja J. (2025). R pacakge `{electoral}`: allocating seats methods
  and party system scores (v0.1.4).
  <https://cran.r-project.org/web/packages/electoral/index.html>
- CEB: Central Election Board (CEB). Some data was scraped from the PDF
  documents provided since 2004 by Central Election Board (CEB) in
  Spain. See <https://www.juntaelectoralcentral.es/cs/jec/elecciones>
- García Guzmán P. (2025). WikiBarrio: Explore Spanish socio-demographic
  data at the neighborhood level. <https://www.wikibarrio.es/>
- García Guzmán P. (2025). ineAtlas: Access to Spanish Household Income
  Distribution Atlas Data. R package version 0.1.3.9000,
  <https://github.com/pablogguz/ineAtlas>
- Meleiro H. (2024). infoelectoral: Download Spanish Election Results. R
  package version 1.0.2, <https://github.com/rOpenSpain/infoelectoral>
- Silge J., Nash J.C., and Graves S. (2018). Navigating the R Package
  Universe. The R Journal 10 (2): 558–63.
  <https://doi.org/10.32614/RJ-2018-058>
- V-Dem Dataset. Varieties of Democracy (V-Dem) Project.
  <https://v-dem.net/data/>
- Wickham H. and Bryan J. R Packages: Organize, Test, Document, and
  Share Your Code (2023). <https://r-pkgs.org/>

## Authors

[Javier Álvarez-Liébana (maintainer)](https://javieralvarezliebana.es),
David Pereiro-Pol, Mafalda González-González, [Irene
Bosque-Gala](https://es.linkedin.com/in/irene-bosque-gala-701271293) and
[Mikaela De
Smedt](https://www.linkedin.com/in/mikaela-de-smedt-11179020a/?locale=en_US).
The development of `{pollspain}` package has been part of several
Master’s Theses from the Master in Computational Data Science at UC3M
(Madrid).

### Usability

The **package’s usability and functionality** have been tested by the
following collaborators:

<details>

<summary>

Contributor database
</summary>

| Contributor | R knowledge | Political science knowledge | Usability score | Functionality score |
|----|----|----|----|----|
| … | 9 | 9 | … | … |
| … | 9 | 2 | … | … |
| … | 5 | 9 | … | … |
| … | 6 | 7 | … | … |
| … | 2 | 3 | … | … |
| … | 6 | 1 | … | … |
| … | 10 | 7 | … | … |
| … | 2 | 8 | … | … |
| … | 5 | 5 | … | … |
| … | 10 | 3 | … | … |
| … | 3 | 9 | … | … |
| … | 9 | 5 | … | … |

</details>
