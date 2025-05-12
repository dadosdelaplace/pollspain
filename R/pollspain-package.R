#' @title \pkg{pollspain} -- A grammar for the analysis of electoral
#' and poll data
#'
#' @description The package is intended to provide a tidyverse-style
#' grammar for preprocessing, debugging, filtering, accessing,
#' downloading and analyzing election data and polls in Spain.
#'
#' @author Javier Álvarez-Liébana, David Pereiro-Pol,
#' Mafalda González-González, Irene Bosque-Gala and Mikaela De Smedt.
#' @references
#' ...
#'
#' @docType package
#' @name pollspain-package
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import tibble
## usethis namespace: start
#' @importFrom dplyr arrange
#' @importFrom dplyr between
#' @importFrom dplyr case_when
#' @importFrom dplyr contains
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom glue glue
#' @importFrom lubridate as_date
#' @importFrom lubridate day
#' @importFrom lubridate dmy
#' @importFrom lubridate is.Date
#' @importFrom lubridate month
#' @importFrom lubridate today
#' @importFrom lubridate year
#' @importFrom readr col_character
#' @importFrom readr cols
#' @importFrom readr locale
#' @importFrom readr parse_number
#' @importFrom readr read_csv
#' @importFrom readr read_lines
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom rvest read_html
#' @importFrom stringi stri_escape_unicode
#' @importFrom stringi stri_trans_general
#' @importFrom tidyr drop_na
#' @importFrom tidyr expand_grid
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @importFrom utils download.file
#' @importFrom utils globalVariables
#' @importFrom utils menu
#' @importFrom utils unzip
## usethis namespace: end
#' @aliases pollspain pollspain-package
"_PACKAGE"
options(pillar.sigfig = 10)
options(dplyr.summarise.inform = FALSE)
