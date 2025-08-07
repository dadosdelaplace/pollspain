#' @title \pkg{pollspain} -- A grammar for the analysis of electoral
#' and poll data
#'
#' @description The package is intended to provide a tidyverse-style
#' grammar for preprocessing, debugging, filtering, accessing,
#' downloading and analyzing election data and polls in Spain.
#'
#' @author Javier Alvarez-Liebana, David Pereiro-Pol,
#' Mafalda Gonzalez-Gonzalez, Irene Bosque-Gala and Mikaela De Smedt.
#' @references
#' ...
#'
#' @docType package
#' @name pollspain-package
#' @import purrr
#' @import stringr
#' @import tibble
## usethis namespace: start
#' @importFrom arrow open_dataset
#' @importFrom arrow read_parquet
#' @importFrom dbplyr sql
#' @importFrom dplyr across
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr between
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr collect
#' @importFrom dplyr contains
#' @importFrom dplyr copy_to
#' @importFrom dplyr count
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr first
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr if_else
#' @importFrom dplyr inner_join
#' @importFrom dplyr last
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr n_distinct
#' @importFrom dplyr pull
#' @importFrom dplyr reframe
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom dplyr right_join
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr slice_max
#' @importFrom dplyr slice_min
#' @importFrom dplyr slice_sample
#' @importFrom dplyr summarise
#' @importFrom dplyr tbl
#' @importFrom dplyr ungroup
#' @importFrom dplyr union_all
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
#' @importFrom stringr str_detect
#' @importFrom stringr str_flatten
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
