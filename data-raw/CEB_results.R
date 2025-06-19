
# ----- packages -----
library(pdftools)
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

    text <- pdf_text(paths)
    lines <- str_split(text[2], "\n")[[1]] |> str_trim()
    lines <- str_squish(str_replace(str_remove_all(lines, "\\.|\\|"), " ", "-"))
    init_tb <- which(str_detect(lines, "^Albacete|^Álava|^Alava"))[1]
    table_raw <- lines[init_tb:length(lines)]
    table <-
      readr::read_table2(str_replace(paste(table_raw, collapse = "\n"),
                                     "Santa-Cruz de Tenerife|Santa Cruz de Tenerife",
                                     "Santa-Cruz-de-Tenerife"),
                         col_names = FALSE) |>
      mutate("X1" = str_trim(str_replace_all(X1, "-", " "))) |>
      filter(!str_detect(str_to_upper(X1), "BOE|CVE|TOTAL ESTATAL") &
               !str_detect(str_to_upper(X2), "BOE|CVE|TOTAL ESTATAL")) |>
      rename(prov = X1, census_counting_prov = X2, total_ballots = X3,
             valid_ballots = X4, party_ballots = X5, blank_ballots = X6,
             invalid_ballots = X7) |>
      mutate("census_counting_prov" = as.numeric(census_counting_prov),
             "total_ballots" = as.numeric(total_ballots),
             "valid_ballots" = as.numeric(valid_ballots),
             "party_ballots" = as.numeric(party_ballots),
             "blank_ballots" = as.numeric(blank_ballots),
             "invalid_ballots" = as.numeric(invalid_ballots)) |>
      mutate("prov" = str_to_title(prov),
             "prov" =
               case_when(str_detect(prov, "Avila") ~ "Ávila",
                         str_detect(prov, "Tenerife") ~
                           "Santa Cruz de Tenerife",
                         str_detect(prov, "Alava|Álava") ~ "Álava/Araba",
                         str_detect(prov, "Guipozcoa|Guipúzcoa|Gipuzkoa") ~
                           "Guipúzcoa/Gipuzkoa",
                         str_detect(prov, "Vizcaya|Bizkaia") ~
                           "Vizcaya/Bizkaia",
                         str_detect(prov, "Navarra|Nafarroa") ~
                           "Navarra/Nafarroa",
                         str_detect(prov, "Orense|Ourense") ~
                           "Orense/Ourense",
                         str_detect(prov, "La Coruña|La Coruna|Coruna|Coruña") ~
                           "La Coruña/A Coruña",
                         str_detect(prov, "Palmas|Palmas (Las)") ~
                           "Las Palmas",
                         str_detect(prov, "Rioja") ~ "La Rioja",
                         str_detect(prov, "Valencia|València") ~
                           "Valencia/València",
                         str_detect(prov, "Castellón|Castellon|Castelló") ~
                           "Castellón/Castelló",
                         str_detect(prov, "Alicante|Alacant") ~
                           "Alicante/Alacant",
                         str_detect(prov, "Baleares|Balears") ~
                           "Islas Baleares/Illes Balears",
                         TRUE ~ prov))

    CEB_results <-
      CEB_results |>
      bind_rows(tibble("cod_elec" = "02",
                       "date" = congress_dates$date[i],
                       "id_elec" = glue("{cod_elec}-{date}"),
                       table))
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


