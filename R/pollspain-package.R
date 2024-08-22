#' @title pollspain
#'
#' @description
#' An R package to access, clean, and analyze electoral and survey data for the Spanish elections from 1982 until present.
#'
#' @details
#' This package includes functionality to work with electoral data, survey data, and various auxiliary datasets related to Spanish elections.
#'
#' @name pollspain-package
#' @docType "_PACKAGE"
#' @aliases pollspain pollspain-package
#'
#' @import dplyr
#' @import stringr
#' @import glue
#' @import readr
#' @import httr
#' @import lubridate
#' @import tidyverse
#' @import stringdist
#' @import stringi
#' @import ggparliament
#' @import ggplot2
#' @import mapSpain
#' @import purrr
#' @import crayon
#' @import rvest
#' @import tidyr
#' @importFrom dplyr arrange
#' @importFrom glue glue
#' @importFrom lubridate as_date day dmy month today year
#' @importFrom readr locale parse_number read_csv read_lines
#' @importFrom rvest html_nodes html_table read_html
#' @importFrom stringi stri_escape_unicode stri_trans_general
#' @importFrom tidyr drop_na pivot_longer pivot_wider replace_na separate unite
#' @importFrom utils download.file globalVariables unzip
NULL

# Ensure to set these options if they are required for your package functionality
options(pillar.sigfig = 13)
options(dplyr.summarise.inform = FALSE)

