
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pollspain

<!-- badges: start -->

[![GitHub
release](https://img.shields.io/github/v/release/dadosdelaplace/pollspain)](https://github.com/dadosdelaplace/pollspain/releases)
<!-- badges: end -->

The main objective of the R package `{pollspain}` is to provide social
scientists, political analysts, journalists, and citizens with easy and
straightforward <span class="hl">**access to electoral data from
Spain**</span>. This includes both **aggregated election results**
extracted from polling stations and **electoral survey data** (including
housing effects). The package also offers simple tools for seat
allocation, vote simulation, and visualization. The package is designed
under <span class="hl">**tidyverse-style functions**</span> specially
tailored for beginner users .

## Installation

You can <span class="hl">**install the development version**</span> from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools") # only if not already installed
devtools::install_github("dadosdelaplace/pollspain")
library(pollspain) # after installing
```

An **internet connection** is required for installing and downloading
data. Data is stored in the accompanying `pollspain-data` repository.
You can find **more information about the data structure** at
<https://github.com/dadosdelaplace/pollspain-data>

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

returns a <span class="hl">**summary table of election results
aggregated at the administrative level**</span>. This includes both
general data (blank votes, turnout, etc) and the **ballots received by
each party or candidacy**. The available <span class="hl">**aggregation
levels**</span> (`level`) are: `"all"` (for a national summary),
`"ccaa"` (autonomous communities), `"prov"` (province), `"mun"`
(municipality), `"mun_district"` (electoral district), `"sec"` (census
section), and `"poll_station"`.

``` r
# Summary election data at national level for both elections in 2019
# (general data without candidacies ballots)
summary_data_all <- summary_election_data(type_elec = "congress", year = 2019)
summary_data_all
```

``` r
# Summary election data, aggregating candidacies ballots at prov level
summary_data_prov_parties <-
  summary_election_data("congress", year = c(2000, 2008, 2023),
                        level = "prov", by_parties = TRUE)
head(summary_data_prov_parties, 5)
```

`summary_election_data()` is a **user-friendly combination** of
`get_election_data()` (which merges different data sources at the
polling station level) and `aggregate_election_data()` (which aggregates
the data to the requested level). See <span class="hl">**some uses and
detailed input arguments**</span> in
[…](https://javieralvarezliebana.es/pollspain/articles/...), and
[…](https://javieralvarezliebana.es/pollspain/articles/...) for
**advanced users**.

<details>

<summary>

<strong>⚠️ About municipalities</strong>
</summary>

The municipality data (names and codes) were **extracted from the
version published by the National Statistics Institute (INE) on February
6, 2025**. The configuration of municipalities from previous years has
been adapted to the most recent setup, recoding cases where
municipalities have merged or disappeared.

Data extracted from
<https://www.ine.es/daco/daco42/codmun/codmun20/20codmun.xlsx>

</details>

<details>

<summary>

<strong>⚠️ About CERA</strong>
</summary>

According to the National Statistics Institute (INE) «the electoral roll
contains the registration of those who meet the requirements to be
voters and are not definitively or temporarily deprived of the right to
vote. The electoral roll is composed of:

- The electoral roll of Spanish citizens residing in Spain (CER).
- The **electoral roll of Spanish citizens residing abroad (CERA)**.

The electoral roll of residents in Spain who are nationals of countries
with Agreements for municipal elections (CERE Agreements), and the
electoral roll of citizens of the European Union residing in Spain for
municipal and European Parliament elections (CERE EU)».

Los datos relativos a CERA se han agregado a nivel nacional, comunidad
autónoma y provincial. …

</details>

### Seat allocation

### Resúmenes de encuestas

### Estimación de encuestas

### Simulación de resultados electorales

### Data viz

- barras ordenadas a más a menos (con colores)
- ggparlament
- encuestas + promedio
- barras con resultados + encuestas encima
- barras con % de voto vs %escaños?
- mapa
- ¿algún lollipop para mostrar housing efects? con flechas y eso.

## Other functions

The `{pollspain}` package also provides <span class="hl">**more advanced
users with useful functions**</span> to preprocess and analyze electoral
data—even their own data, as long as it is provided in a proper format.

- <span class="hl">**Utils**</span>: functions contained in the
  `utils.R` script are intended to serve as **helper functions for data
  preprocessing**. See \<…\> for more examples about how to use them.

``` r
type_to_code_election(type_elec = "congress")
#> [1] "02"
extract_code("01-04-003-01-004-B", level = "mun")
#> [1] "003"
extract_code("01-04-003-01-004-B", level = "mun", full_cod = TRUE)
#> [1] "01-04-003"
```

- \[**Import raw data**\]\]{.hl}: functions starting with
  `import_..._data()` (code can be found in the
  `import_elections_data.R` file) are aimed at importing and
  preprocessing as raw as possible the `.DAT` election files from the
  Spanish Ministry of Interior files available in the Github repository
  . See \<…\> for more examples about how to use them.

``` r
# import and preprocess elections data at poll stations level for given election
# types and dates, providing variables related to turnout, blank/valid votes, etc
poll_data <- import_poll_station_data(type_elec = "congress", year = 2019)
head(poll_data)
```

## Authors

**Javier Álvarez-Liébana (maintainer)**, **David Pereiro-Pol**,
**Mafalda González-González**, **Irene Bosque-Gala** and **Mikaela De
Smedt**.

`{pollspain}` package has been part of several Master’s Theses from the
Master in Computational Data Science at UC3M (Madrid). The package’s
usability and functionality have been tested by the following
collaborators:

## References

This package has been designed based on the **following resources and
references**

- Albuja J. (2025). R pacakge `{electoral}`: allocating seats methods
  and party system scores (v0.1.4).
  <https://cran.r-project.org/web/packages/electoral/index.html>

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

- Wickham H. and Bryan J. R Packages: Organize, Test, Document, and
  Share Your Code (2023). <https://r-pkgs.org/>

- Electoral data download from repository of the Spanish Ministry of the
  Interior.
  <https://infoelectoral.interior.gob.es/es/elecciones-celebradas/area-de-descargas/>
