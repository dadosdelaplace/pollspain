
# ----- packages -----
library(tabulapdf)
library(tidyverse)
library(glue)
library(lubridate)
library(readr)

# ----- download pdf info -----
congress_dates <- dates_elections_spain |> filter(cod_elec == "02")
CEB_results <- tibble()
for (i in 1:nrow(congress_dates)) {

  if (!is.na(congress_dates$pdf_CEB[i])) {

    paths <-
      system.file("extdata", glue("CEB_pdf/CEB_{congress_dates$date[i]}.pdf"),
                  package = "pollspaindata")
    download.file(paths,
                  glue("./data/CEB_pdf/CEB_{congress_dates$date[i]}.pdf"),
                  mode = "wb")

    # Extract from first pages
    if (congress_dates$date[i] == "2019-04-28") { idx_pages <- 2:3 } else { idx_pages <- 2}
    tables_pdf <-
      extract_tables(paths, pages = idx_pages,
                     guess = TRUE, output = "tibble") |>
      bind_rows() |>
      mutate(across(everything(), function(x) { str_replace_all(x, "\\.", "")})) |>
      janitor::clean_names()

    if (congress_dates$year[i] == 2023) {

      tables_pdf <-
        tables_pdf |>
        separate(col = electores_votantes, into = c("electores", "votantes"))

    }

    if (congress_dates$year[i] == 2015) {

      tables_pdf <-
        tables_pdf |>
        separate(col = 2, into = c("electores", "votantes")) |>
        separate(col = "x3", into = c("validos", "partidos", "blanco", "invalido"))

    }

    if (congress_dates$year[i] == 2011) {

      tables_pdf <-
        tables_pdf |>
        slice(-(1:3))
    }

    names(tables_pdf) <-
      c("prov", "census_counting_prov", "total_ballots",
        "valid_ballots", "party_ballots", "blank_ballots",
        "invalid_ballots")

    tables_pdf <-
      tables_pdf |>
      mutate(across(everything(), function(x) { str_squish(str_replace_all(x, "\\|", "")) })) |>
      mutate(across(where(is.character), function(x) { str_replace_all(x, "\\s", "") })) |>
      mutate("prov" = str_to_title(prov),
             "prov" =
               case_when(str_detect(str_to_lower(prov), "girona") ~
                           "Girona",
                         str_detect(str_to_lower(prov), "gipuz") ~
                           "Guipúzcoa/Gipuzkoa",
                         str_detect(str_to_lower(prov), "real") ~
                           "Ciudad Real",
                         str_detect(str_to_lower(prov), "baleares|balears") ~
                           "Islas Baleares/Illes Balears",
                         str_detect(str_to_lower(prov), "alicante|alacant") ~
                           "Alicante/Alacant",
                         str_detect(str_to_lower(prov), "castellón|castelló|castello") ~
                           "Castellón/Castelló",
                         str_detect(str_to_lower(prov), "valencia|valència") ~
                           "Valencia/València",
                         str_detect(str_to_lower(prov), "coruña|coruna") ~
                           "La Coruña/A Coruña",
                         str_detect(str_to_lower(prov), "orense|ourense|oure") ~
                           "Orense/Ourense",
                         str_detect(str_to_lower(prov), "navarra|nafarroa") ~
                           "Navarra/Nafarroa",
                         str_detect(str_to_lower(prov), "álava|alava|araba") ~
                           "Álava/Araba",
                         str_detect(str_to_lower(prov), "vizcaya|bizkaia") ~
                           "Vizcaya/Bizkaia",
                         str_detect(str_to_lower(prov), "rioja") ~
                           "La Rioja",
                         str_detect(str_to_lower(prov), "palmas|palbmlaes|picaalmbales") ~
                           "Las Palmas",
                         str_detect(str_to_lower(prov), "tenerife") ~
                           "Santa Cruz de Tenerife",
                         str_detect(str_to_lower(prov), "palencia|paoleen-caia|paoleen caia|pbaolenec-iaa") ~
                           "Palencia",
                         TRUE ~ prov),
             "prov" = str_trim(str_replace_all(prov, "-", " "))) |>
      filter(!str_detect(str_to_upper(prov),
                         "BOE|CVE|ESTATAL|VERIFICABLE")) |>
      mutate("census_counting_prov" = as.numeric(census_counting_prov),
             "total_ballots" = as.numeric(total_ballots),
             "valid_ballots" = as.numeric(valid_ballots),
             "party_ballots" = as.numeric(party_ballots),
             "blank_ballots" = as.numeric(blank_ballots),
             "invalid_ballots" = as.numeric(invalid_ballots))

    CEB_results <-
      CEB_results |>
      bind_rows(tibble("cod_elec" = "02",
                       "date" = congress_dates$date[i],
                       "id_elec" = glue("{cod_elec}-{date}"),
                       tables_pdf))
  }
}
CEB_results <-
  CEB_results |>
  left_join(cod_INE_prov_ccaa, by = "prov") |>
  mutate("id_INE_prov" = glue("{cod_INE_ccaa}-{cod_INE_prov}"))

# ----- UTF-8 -----

CEB_results <-
  CEB_results |>
  mutate(across(where(is.character), \(x) enc2utf8(x)))

# ----- use data -----
usethis::use_data(CEB_results, overwrite = TRUE,
                  compress = "xz")

# ----- write_csv -----
# write_csv(CEB_results, "./data/csv/CEB_results.csv")


