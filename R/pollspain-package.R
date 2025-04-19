#' @title \pkg{pollspain} -- blabla
#'
#' @description bla bla
#'
#' @author Javier Álvarez-Liébana.
#' @references
#' García-Portugués, E., Álvarez-Liébana, J., Álvarez-Pérez, G. and
#' González-Manteiga, W. (2019). A goodness-of-fit test for the functional
#' linear model with functional response. \emph{arXiv:1909.07686}.
#' \url{https://arxiv.org/abs/1909.07686}
#'
#' García-Portugués, E., González-Manteiga, W. and Febrero-Bande, M. (2014). A
#' goodness-of-fit test for the functional linear model with scalar response.
#' \emph{Journal of Computational and Graphical Statistics}, 23(3):761--778.
#' \url{http://doi.org/10.1080/10618600.2013.812519}
#' @docType package
#' @name pollspain-package
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import tibble
## usethis namespace: start
#' @importFrom dplyr arrange
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @importFrom glue glue
#' @importFrom lubridate as_date
#' @importFrom lubridate day
#' @importFrom lubridate dmy
#' @importFrom lubridate is.Date
#' @importFrom lubridate month
#' @importFrom lubridate today
#' @importFrom lubridate year
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
#' @importFrom utils unzip
## usethis namespace: end
#' @aliases pollspain pollspain-package
NULL
options(pillar.sigfig = 13)
options(dplyr.summarise.inform = FALSE)
