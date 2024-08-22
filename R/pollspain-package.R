#' @title pollspain
#'
#' @description R package to access, clean, and analyze electoral and survey data
#' for the Spanish elections from 1982 until present.
#'
#' @author MSc Mikaela De Smedt, PhD Javier Alvarez-Liebana
#' @maintainer Mikaela De Smedt (mkldesmedt@gmail.com), Javier Alvarez-Liebana (javalv09@ucm.es)
#' @license MIT
#' @version 1.0
#' @imports
#' dplyr, stringr, glue, readr, httr, lubridate, tidyverse, stringdist, stringi, ggparliament, ggplot2, mapSpain
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
#' @importFrom dplyr arrange
#' @importFrom glue glue
#' @importFrom lubridate as_date day dmy month today year
#' @importFrom readr locale parse_number read_csv read_lines
#' @importFrom rvest html_nodes html_table read_html
#' @importFrom stringi stri_escape_unicode stri_trans_general
#' @importFrom tidyr drop_na pivot_longer pivot_wider replace_na separate unite
#' @importFrom utils download.file globalVariables unzip
#' @docType package
#' @name pollspain-package
#' @aliases pollspain pollspain-package
NULL

options(pillar.sigfig = 13)
options(dplyr.summarise.inform = FALSE)

