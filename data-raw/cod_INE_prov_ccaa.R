
# ----- packages -----
library(tibble)
library(dplyr)
library(readxl)
library(readr)
library(glue)

# ----- codes -----

# INE's code for ccaa
cod_INE_ccaa <-
  c("01", "01", "01", "01", "01", "01", "01", "01",
    "02", "02", "02", "03", "04", "05", "05", "06",
    "07", "07", "07", "07", "07", "07", "07", "07", "07",
    "08", "08", "08", "08", "08",
    "09", "09", "09", "09",
    "10", "10", "10", "11", "11",
    "12", "12", "12", "12",
    "13", "14", "15", "16", "16", "16", "17",
    "18", "19")

cod_MIR_ccaa <-
  c("01", "01", "01", "01", "01", "01", "01", "01",
    "02", "02", "02", "03", "04", "05", "05", "06",
    "08", "08", "08", "08", "08", "08", "08", "08", "08",
    "07", "07", "07", "07", "07",
    "09", "09", "09", "09",
    "17", "17", "17", "10", "10",
    "11", "11", "11", "11",
    "12", "15", "13", "14", "14", "14", "16",
    "18", "19")

ccaa <-
  c("Andalucía", "Andalucía", "Andalucía", "Andalucía",
    "Andalucía", "Andalucía", "Andalucía", "Andalucía",
    "Aragón", "Aragón", "Aragón", "Principado de Asturias",
    "Islas Baleares (Illes Balears)", "Canarias", "Canarias", "Cantabria",
    "Castilla y León", "Castilla y León", "Castilla y León",
    "Castilla y León", "Castilla y León", "Castilla y León",
    "Castilla y León", "Castilla y León", "Castilla y León",
    "Castilla-La Mancha", "Castilla-La Mancha",
    "Castilla-La Mancha", "Castilla-La Mancha",
    "Castilla-La Mancha", "Cataluña (Catalunya)", "Cataluña (Catalunya)",
    "Cataluña (Catalunya)", "Cataluña (Catalunya)",
    "Comunidad Valenciana (Comunitat Valenciana)",
    "Comunidad Valenciana (Comunitat Valenciana)",
    "Comunidad Valenciana (Comunitat Valenciana)",
    "Extremadura", "Extremadura", "Galicia (Galiza)",
    "Galicia (Galiza)", "Galicia (Galiza)", "Galicia (Galiza)",
    "Comunidad de Madrid", "Región de Murcia",
    "Comunidad Foral de Navarra ", "País Vasco (Euskadi)",
    "País Vasco (Euskadi)", "País Vasco (Euskadi)",
    "La Rioja", "Ceuta", "Melilla")

# INE's code for provinces
cod_INE_prov <-
  c("04", "11", "14", "18", "21", "23", "29", "41",
    "22", "44", "50", "33", "07", "35", "38", "39",
    "05", "09", "24", "34", "37", "40", "42", "47", "49",
    "02", "13", "16", "19", "45",
    "08", "17", "25", "43",
    "03", "12", "46", "06", "10",
    "15", "27", "32", "36",
    "28", "30", "31", "01", "48", "20", "26",
    "51", "52")
prov <-
  c("Almería", "Cádiz", "Córdoba", "Granada", "Huelva",
    "Jaén", "Málaga", "Sevilla", "Huesca", "Teruel",
    "Zaragoza", "Asturias", "Islas Baleares/Illes Balears",
    "Las Palmas", "Santa Cruz de Tenerife", "Cantabria",
    "Ávila", "Burgos", "León", "Palencia", "Salamanca",
    "Segovia", "Soria", "Valladolid", "Zamora", "Albacete",
    "Ciudad Real", "Cuenca", "Guadalajara", "Toledo",
    "Barcelona", "Girona", "Lleida", "Tarragona",
    "Alicante/Alacant", "Castellón/Castelló",
    "Valencia/València", "Badajoz", "Cáceres", "La Coruña/A Coruña",
    "Lugo", "Orense/Ourense", "Pontevedra", "Madrid", "Murcia",
    "Navarra/Nafarroa", "Álava/Araba", "Vizcaya/Bizkaia",
    "Guipúzcoa/Gipuzkoa", "La Rioja", "Ceuta", "Melilla")

# Build tibbles
cod_INE_prov_ccaa <-
  tibble(cod_INE_ccaa, cod_MIR_ccaa, ccaa, cod_INE_prov, prov)

cod_INE_prov <-
  cod_INE_prov_ccaa |>
  select(contains("_prov"))

cod_INE_ccaa <-
  cod_INE_prov_ccaa |>
  distinct(cod_INE_ccaa, .keep_all = TRUE) |>
  select(contains("ccaa"))

# ----- muni's codes

cod_INE_mun <-
  read_csv(file = "./data/cod_INE_mun.csv") |>
  # Renamee INE cols
  rename(cod_INE_ccaa = CODAUTO, cod_INE_prov = CPRO,
         cod_INE_mun = CMUN, cd_INE_mun = DC,
         mun = NOMBRE) |>
  left_join(cod_INE_prov_ccaa, by = c("cod_INE_ccaa", "cod_INE_prov")) |>
  # Create id codes
  mutate(id_INE_mun =
           glue("{cod_INE_ccaa}-{cod_INE_prov}-{cod_INE_mun}"),
         id_MIR_mun =
           glue("{cod_MIR_ccaa}-{cod_INE_prov}-{cod_INE_mun}")) |>
  relocate(id_INE_mun, id_MIR_mun, .before = everything()) |>
  relocate(cod_MIR_ccaa, .after = cod_INE_ccaa) |>
  mutate("mun" = glue("{mun} ({prov})"))

# ----- use data: rda -----

usethis::use_data(cod_INE_prov_ccaa, overwrite = TRUE,
                  compress = "gzip")
usethis::use_data(cod_INE_prov, overwrite = TRUE,
                  compress = "gzip")
usethis::use_data(cod_INE_ccaa, overwrite = TRUE,
                  compress = "gzip")
usethis::use_data(cod_INE_mun, overwrite = TRUE,
                  compress = "gzip")

# ----- write_csv -----

write_csv(cod_INE_mun, "./data/cod_INE_mun.csv")



