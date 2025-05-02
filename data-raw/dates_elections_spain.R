

# ----- packages -----

library(tidyverse)
library(glue)
library(lubridate)
library(readr)

# Referendums
ref_elections_spain <-
  tibble(cod_elec = "01", type_elec = "referendum",
         date = as_date(c("1976-12-15", "1978-12-06",
                          "1986-03-12", "2005-02-20")),
         topic = c("Ref. Proyecto Ley Reforma Política",
                   "Constitución", "OTAN", "Constitución UE"))

# Congress
congress_elections_spain <-
  tibble(cod_elec = "02", type_elec = "congress",
         date =
           as_date(c("1982-10-28",
                     "1986-06-22", "1989-10-29", "1993-06-06",
                     "1996-03-03", "2000-03-12", "2004-03-14",
                     "2008-03-09", "2011-11-20", "2015-12-20",
                     "2016-06-26", "2019-04-28", "2019-11-10",
                     "2023-07-24")))

# Senate
senate_elections_spain <-
  tibble(cod_elec = "03", type_elec = "senate",
         date =
           as_date(c("1982-10-28",
                     "1986-06-22", "1989-10-29", "1993-06-06",
                     "1996-03-03", "2000-03-12", "2004-03-14",
                     "2008-03-09", "2011-11-20", "2015-12-20",
                     "2016-06-26", "2019-04-28", "2019-11-10",
                     "2023-07-24")))

# Muni
mun_elections_spain <-
  tibble(cod_elec = "04", type_elec = "local",
         date =
           as_date(c("1983-05-08", "1987-06-10",
                     "1991-05-26", "1995-05-28", "1999-06-13",
                     "2003-05-28", "2007-05-27", "2011-05-22",
                     "2015-05-24", "2019-05-26", "2023-05-28")))

# Cabildos
cabildo_elections_spain <-
  tibble(cod_elec = "06", type_elec = "cabildo",
         date =
           as_date(c("1983-05-08", "1987-06-10",
                     "1991-05-26", "1995-05-28", "1999-06-13",
                     "2003-05-28", "2007-05-27", "2011-05-22",
                     "2015-05-24", "2019-05-26")))

# EU
euro_elections_spain <-
  tibble(cod_elec = "07", type_elec = "EU",
         date =
           as_date(c("1987-06-10", "1989-06-15",
                     "1994-06-12", "1999-06-13",
                     "2004-06-13", "2009-06-07",
                     "2014-05-25", "2019-05-26",
                     "2024-06-09")))

# Concatenate by rows
dates_elections_spain <-
  bind_rows(ref_elections_spain, congress_elections_spain,
            senate_elections_spain, mun_elections_spain,
            cabildo_elections_spain, euro_elections_spain) %>%
  mutate(year = year(date), month = month(date), day = day(date)) %>%
  relocate(topic, .after = everything())

# ----- use data -----
usethis::use_data(dates_elections_spain, overwrite = TRUE,
                  compress = "gzip")

# ----- write_csv -----
# write_csv(dates_elections_spain, "./data/csv/dates_elections_spain.csv")
