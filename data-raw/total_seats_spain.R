
# ----- required libraries -----

library(dplyr)
library(tidyr)
library(stringr)
library(pollspain)

# ----- seats ------
seats_1982 <- tibble(
  prov = c(
    "Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
    "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
    "Islas Baleares/Illes Balears", "Las Palmas",
    "Santa Cruz de Tenerife", "Cantabria", "Albacete", "Ciudad Real",
    "Cuenca", "Guadalajara", "Toledo", "Ávila", "Burgos", "León",
    "Palencia", "Salamanca", "Segovia", "Soria", "Valladolid", "Zamora",
    "Barcelona", "Girona", "Lleida", "Tarragona", "Ceuta",
    "Alicante/Alacant", "Castellón/Castelló", "Valencia/València",
    "Badajoz", "Cáceres", "La Coruña/A Coruña", "Lugo",
    "Orense/Ourense", "Pontevedra", "La Rioja", "Madrid", "Melilla",
    "Murcia", "Navarra/Nafarroa", "Álava/Araba", "Guipúzcoa/Gipuzkoa",
    "Vizcaya/Bizkaia"
  ),
  nseats = c(
    5,  8, 7, 7, 5, 7, 8, 12, 3, 3, 8, 10,
    6,  6, 7, 5, 4, 5, 4, 3, 5, 3, 4, 6,
    3,  4, 3, 3, 5, 4, 33, 5, 4, 5, 1,
    9,  5, 15, 7, 5, 9, 5, 5, 8, 4, 32,
    1,  8, 5, 4, 7, 10
  ),
  id_elec = "02-1982-10-28")



seats_1986 <- tibble(
  prov = c(
    "Álava/Araba", "Albacete", "Alicante/Alacant", "Almería", "Asturias", "Ávila",
    "Badajoz", "Islas Baleares/Illes Balears", "Barcelona", "Burgos", "Cáceres",
    "Cádiz", "Cantabria", "Castellón/Castelló", "Ciudad Real", "Córdoba",
    "La Coruña/A Coruña", "Cuenca", "Girona", "Granada", "Guadalajara",
    "Guipúzcoa/Gipuzkoa", "Huelva", "Huesca", "Jaén", "León", "Lleida", "Lugo",
    "Madrid", "Málaga", "Murcia", "Navarra/Nafarroa", "Orense/Ourense",
    "Palencia", "Las Palmas", "Pontevedra", "La Rioja", "Salamanca",
    "Santa Cruz de Tenerife", "Segovia", "Sevilla", "Soria", "Tarragona",
    "Teruel", "Toledo", "Valencia/València", "Valladolid", "Vizcaya/Bizkaia",
    "Zamora", "Zaragoza", "Ceuta", "Melilla"
  ),
  nseats = c(
    4, 4, 10, 5, 9, 3,  6, 6, 33, 4, 5, 9, 5, 5, 5, 7, 9, 3, 5, 7, 3, 7, 5, 3,
    6, 5,  4, 5, 33, 9, 8, 5,  5, 3, 7, 8, 4, 4, 6, 3, 12, 3, 5, 3, 5, 16, 5,
    10, 4,  8, 1, 1
  ),
  id_elec = "02-1986-06-22")



seats_1989 <- tibble(
  prov = c(
    "Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
    "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
    "Islas Baleares/Illes Balears", "Las Palmas", "Santa Cruz de Tenerife",
    "Cantabria", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara",
    "Toledo", "Ávila", "Burgos", "León", "Palencia", "Salamanca",
    "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona",
    "Lleida", "Tarragona", "Ceuta", "Alicante/Alacant",
    "Castellón/Castelló", "Valencia/València", "Badajoz", "Cáceres",
    "La Coruña/A Coruña", "Lugo", "Orense/Ourense", "Pontevedra",
    "La Rioja", "Madrid", "Melilla", "Murcia",
    "Navarra/Nafarroa", "Álava/Araba", "Guipúzcoa/Gipuzkoa",
    "Vizcaya/Bizkaia"
  ),
  nseats = c(
    5,  9, 7, 7, 5, 6, 10, 12, 3, 3, 7, 9,
    6,  7, 7, 5, 4, 5, 3, 3, 5, 3, 4, 5,
    3,  4, 3, 3, 5, 3, 32, 5, 4, 5, 1, 10,
    5, 16, 6, 5, 9, 5, 5, 8, 4, 33, 1, 9,
    5,  4, 7, 10
  ),
  id_elec = "02-1989-10-29")


seats_1993 <- tibble(
  prov = c(
    "Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
    "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
    "Islas Baleares/Illes Balears", "Las Palmas", "Santa Cruz de Tenerife",
    "Cantabria", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara",
    "Toledo", "Ávila", "Burgos", "León", "Palencia", "Salamanca",
    "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona",
    "Lleida", "Tarragona", "Ceuta", "Alicante/Alacant", "Castellón/Castelló",
    "Valencia/València", "Badajoz", "Cáceres", "La Coruña/A Coruña",
    "Lugo", "Orense/Ourense", "Pontevedra", "La Rioja", "Madrid",
    "Melilla", "Murcia", "Navarra/Nafarroa", "Álava/Araba",
    "Guipúzcoa/Gipuzkoa", "Vizcaya/Bizkaia"
  ),
  nseats = c(
    5, 9, 7, 7, 5, 6, 10, 12, 3, 3, 7, 9,
    7, 7, 7, 5, 4, 5, 3, 3, 5, 3, 4, 5,
    3, 4, 3, 3, 5, 3, 32, 5, 4, 6, 1, 10,
    5, 16, 6, 5, 9, 5, 4, 8, 4, 34, 1, 9,
    5, 4, 6, 9
  ),
  id_elec = "02-1993-06-06"
)


seats_1996 <- tibble(
  prov = c(
    "Álava/Araba", "Albacete", "Alicante/Alacant", "Almería", "Asturias", "Ávila",
    "Badajoz", "Islas Baleares/Illes Balears", "Barcelona", "Burgos", "Cáceres",
    "Cádiz", "Cantabria", "Castellón/Castelló", "Ceuta", "Ciudad Real",
    "Córdoba", "La Coruña/A Coruña", "Cuenca", "Girona", "Granada",
    "Guadalajara", "Guipúzcoa/Gipuzkoa", "Huelva", "Huesca", "Jaén",
    "León", "Lleida", "Lugo", "Madrid", "Málaga", "Melilla", "Murcia",
    "Navarra/Nafarroa", "Orense/Ourense", "Palencia", "Las Palmas",
    "Pontevedra", "La Rioja", "Salamanca", "Santa Cruz de Tenerife",
    "Segovia", "Sevilla", "Soria", "Tarragona", "Teruel", "Toledo",
    "Valencia/València", "Valladolid", "Vizcaya/Bizkaia", "Zamora",
    "Zaragoza"
  ),
  nseats = c(
    4, 4, 11, 5, 9, 3, 6, 7, 31, 4, 5, 9, 5, 5, 1, 5, 7, 9, 3, 5,
    7, 3, 6, 5, 3, 6, 5, 4, 4, 34, 10, 1, 9, 5, 4, 3, 7, 8, 4, 4,
    7, 3, 13, 3, 6, 3, 5, 16, 5, 9, 3, 7
  ),
  id_elec = "02-1996-03-03"
)


seats_2000 <- tibble(
  prov = c(
    "Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
    "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
    "Islas Baleares/Illes Balears", "Las Palmas", "Santa Cruz de Tenerife",
    "Cantabria", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara",
    "Toledo", "Ávila", "Burgos", "León", "Palencia", "Salamanca",
    "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona",
    "Lleida", "Tarragona", "Ceuta", "Alicante/Alacant", "Castellón/Castelló",
    "Valencia/València", "Badajoz", "Cáceres", "La Coruña/A Coruña",
    "Lugo", "Orense/Ourense", "Pontevedra", "La Rioja", "Madrid",
    "Melilla", "Murcia", "Navarra/Nafarroa", "Álava/Araba",
    "Guipúzcoa/Gipuzkoa", "Vizcaya/Bizkaia"
  ),
  nseats = c(
    5,  9, 7, 7, 5, 6, 10, 13, 3, 3, 7, 9,
    7,  7, 7, 5, 4, 5, 3, 3, 5, 3, 4, 5,
    3,  4, 3, 3, 5, 3, 31, 5, 4, 6, 1, 11,
    5, 16, 6, 5, 9, 4, 4, 8, 4, 34, 1, 9,
    5,  4, 6, 9
  ),
  id_elec = "02-2000-03-12")


seats_2004 <- tibble(
  prov = c(
    "Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
    "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
    "Islas Baleares/Illes Balears", "Las Palmas", "Santa Cruz de Tenerife",
    "Cantabria", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara",
    "Toledo", "Ávila", "Burgos", "León", "Palencia", "Salamanca",
    "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona",
    "Lleida", "Tarragona", "Ceuta", "Alicante/Alacant", "Castellón/Castelló",
    "Valencia/València", "Badajoz", "Cáceres", "La Coruña/A Coruña",
    "Lugo", "Orense/Ourense", "Pontevedra", "La Rioja", "Madrid",
    "Melilla", "Murcia", "Navarra/Nafarroa", "Álava/Araba",
    "Guipúzcoa/Gipuzkoa", "Vizcaya/Bizkaia"
  ),
  nseats = c(
    5,  9, 7, 7, 5, 6, 10, 12, 3, 3, 7, 8,
    8,  8, 7, 5, 4, 5, 3, 3, 5, 3, 4, 5,
    3,  4, 3, 3, 5, 3, 31, 6, 4, 6, 1, 11,
    5, 16, 6, 4, 9, 4, 4, 7, 4, 35, 1, 9,
    5,  4, 6, 9
  ),
  id_elec = "02-2004-03-14")


seats_2008 <- tibble(
  prov = c(
    "Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén",
    "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza", "Asturias",
    "Islas Baleares/Illes Balears", "Las Palmas", "Santa Cruz de Tenerife",
    "Cantabria", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara",
    "Toledo", "Ávila", "Burgos", "León", "Palencia", "Salamanca",
    "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona",
    "Lleida", "Tarragona", "Ceuta", "Alicante/Alacant", "Castellón/Castelló",
    "Valencia/València", "Badajoz", "Cáceres", "La Coruña/A Coruña",
    "Lugo", "Orense/Ourense", "Pontevedra", "La Rioja", "Madrid",
    "Melilla", "Murcia", "Navarra/Nafarroa", "Álava/Araba",
    "Guipúzcoa/Gipuzkoa", "Vizcaya/Bizkaia"
  ),
  nseats = c(
    6,  9, 6, 7, 5, 6, 10, 12, 3, 3, 7, 8,
    8,  8, 7, 5, 4, 5, 3, 3, 6, 3, 4, 5,
    3,  4, 3, 2, 5, 3, 31, 6, 4, 6, 1, 12,
    5, 16, 6, 4, 8, 4, 4, 7, 4, 35, 1, 10,
    5,  4, 6, 8
  ),
  id_elec = "02-2008-03-09")


seats_2011 <- tibble(
  prov = c(
    "Álava/Araba", "Albacete", "Alicante/Alacant", "Almería", "Asturias", "Ávila",
    "Badajoz", "Islas Baleares/Illes Balears", "Barcelona", "Burgos", "Cáceres",
    "Cádiz", "Cantabria", "Castellón/Castelló", "Ciudad Real", "Córdoba",
    "La Coruña/A Coruña", "Cuenca", "Girona", "Granada", "Guadalajara",
    "Guipúzcoa/Gipuzkoa", "Huelva", "Huesca", "Jaén", "León", "Lleida", "Lugo",
    "Madrid", "Málaga", "Murcia", "Navarra/Nafarroa", "Orense/Ourense",
    "Palencia", "Las Palmas", "Pontevedra", "La Rioja", "Salamanca",
    "Santa Cruz de Tenerife", "Segovia", "Sevilla", "Soria", "Tarragona",
    "Teruel", "Toledo", "Valencia/València", "Valladolid", "Vizcaya/Bizkaia",
    "Zamora", "Zaragoza", "Ceuta", "Melilla"
  ),
  nseats = c(
    4, 4, 12, 6, 8, 3, 6, 8, 31, 4, 4, 8, 5, 5, 5, 6, 8, 3, 6, 7,
    3, 6, 5, 3, 6, 5, 4, 4, 36, 10, 10, 5, 4, 3, 8, 7, 4, 4, 7, 3,
    12, 2, 6, 3, 6, 16, 5, 8, 3, 7, 1, 1
  ),
  id_elec = "02-2011-11-20")


seats_2015 <- tibble(
  prov = c(
    "Álava/Araba", "Albacete", "Alicante/Alacant", "Almería", "Asturias", "Ávila",
    "Badajoz", "Islas Baleares/Illes Balears", "Barcelona", "Burgos", "Cáceres",
    "Cádiz", "Cantabria", "Castellón/Castelló", "Ceuta", "Ciudad Real",
    "Córdoba", "La Coruña/A Coruña", "Cuenca", "Girona", "Granada",
    "Guadalajara", "Guipúzcoa/Gipuzkoa", "Huelva", "Huesca", "Jaén",
    "León", "Lleida", "Lugo", "Madrid", "Málaga", "Melilla", "Murcia",
    "Navarra/Nafarroa", "Orense/Ourense", "Palencia", "Las Palmas",
    "Pontevedra", "La Rioja", "Salamanca", "Santa Cruz de Tenerife",
    "Segovia", "Sevilla", "Soria", "Tarragona", "Teruel", "Toledo",
    "Valencia/València", "Valladolid", "Vizcaya/Bizkaia", "Zamora",
    "Zaragoza"
  ),
  nseats = c(
    4, 4, 12, 6, 8, 3, 6, 8, 31, 4, 4, 9, 5, 5, 1, 5, 6, 8, 3, 6,
    7, 3, 6, 5, 3, 5, 5, 4, 4, 36, 11, 1, 10, 5, 4, 3, 8, 7, 4, 4,
    7, 3, 12, 2, 6, 3, 6, 15, 5, 8, 3, 7
  ),
  id_elec = "02-2015-12-20")

seats_2016 <- tibble(
  prov = c(
    "Álava/Araba", "Albacete", "Alicante/Alacant", "Almería", "Asturias", "Ávila",
    "Badajoz", "Islas Baleares/Illes Balears", "Barcelona", "Burgos", "Cáceres",
    "Cádiz", "Cantabria", "Castellón/Castelló", "Ceuta", "Ciudad Real", "Córdoba",
    "La Coruña/A Coruña", "Cuenca", "Girona", "Granada", "Guadalajara",
    "Guipúzcoa/Gipuzkoa", "Huelva", "Huesca", "Jaén", "León", "Lleida", "Lugo",
    "Madrid", "Málaga", "Melilla", "Murcia", "Navarra/Nafarroa", "Orense/Ourense",
    "Palencia", "Las Palmas", "Pontevedra", "La Rioja", "Salamanca",
    "Santa Cruz de Tenerife", "Segovia", "Sevilla", "Soria", "Tarragona",
    "Teruel", "Toledo", "Valencia/València", "Valladolid", "Vizcaya/Bizkaia",
    "Zamora", "Zaragoza"
  ),
  nseats = c(
    4, 4, 12, 6, 8, 3, 6, 8, 31, 4, 4, 9, 5, 5, 1, 5, 6, 8, 3, 6, 7, 3, 6, 5, 3,
    5, 4, 4, 4, 36, 11, 1, 10, 5, 4, 3, 8, 7, 4, 4, 7, 3, 12, 2, 6, 3, 6, 16,
    5, 8, 3, 7
  ),
  id_elec = "02-2016-06-26")

seats_2019a <- tibble(
  prov = c(
    "Álava/Araba", "Albacete", "Alicante/Alacant", "Almería", "Asturias", "Ávila",
    "Badajoz", "Islas Baleares/Illes Balears", "Barcelona", "Burgos", "Cáceres",
    "Cádiz", "Cantabria", "Castellón/Castelló", "Ceuta", "Ciudad Real",
    "Córdoba", "La Coruña/A Coruña", "Cuenca", "Girona", "Granada",
    "Guadalajara", "Guipúzcoa/Gipuzkoa", "Huelva", "Huesca", "Jaén",
    "León", "Lleida", "Lugo", "Madrid", "Málaga", "Melilla", "Murcia",
    "Navarra/Nafarroa", "Orense/Ourense", "Palencia", "Las Palmas",
    "Pontevedra", "La Rioja", "Salamanca", "Santa Cruz de Tenerife",
    "Segovia", "Sevilla", "Soria", "Tarragona", "Teruel", "Toledo",
    "Valencia/València", "Valladolid", "Vizcaya/Bizkaia", "Zamora",
    "Zaragoza"
  ),
  nseats = c(
    4, 4, 12, 6, 7, 3, 6, 8, 32, 4, 4, 9, 5, 5, 1, 5, 6, 8, 3, 6,
    7, 3, 6, 5, 3, 5, 4, 4, 4, 37, 11, 1, 10, 5, 4, 3, 8, 7, 4, 4,
    7, 3, 12, 2, 6, 3, 6, 15, 5, 8, 3, 7
  ),
  id_elec = "02-2019-04-28")

seats_2019n <- tibble(
  prov = c(
    "Madrid", "Barcelona", "Valencia/València", "Alicante/Alacant", "Sevilla",
    "Málaga", "Murcia", "Cádiz", "La Coruña/A Coruña", "Vizcaya/Bizkaia",
    "Islas Baleares/Illes Balears", "Las Palmas", "Asturias", "Granada",
    "Pontevedra", "Santa Cruz de Tenerife", "Zaragoza", "Almería", "Badajoz",
    "Córdoba", "Girona", "Guipúzcoa/Gipuzkoa", "Tarragona", "Toledo",
    "Cantabria", "Castellón/Castelló", "Ciudad Real", "Huelva", "Jaén",
    "Navarra/Nafarroa", "Valladolid", "Álava/Araba", "Albacete", "Burgos",
    "Cáceres", "León", "Lleida", "Lugo", "Orense/Ourense", "La Rioja",
    "Salamanca", "Ávila", "Cuenca", "Guadalajara", "Huesca", "Palencia",
    "Segovia", "Teruel", "Zamora", "Soria", "Ceuta", "Melilla"
  ),
  nseats = c(
    37, 32, 15, 12, 12, 11, 10, 9, 8, 8, 8, 8, 7, 7, 7, 7, 7, 6, 6, 6,
    6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 1
  ),
  id_elec = "02-2019-11-10")


seats_2023 <- tibble(
  prov = c(
    "Madrid", "Barcelona", "Valencia/València", "Alicante/Alacant", "Sevilla",
    "Málaga", "Murcia", "Cádiz", "La Coruña/A Coruña", "Vizcaya/Bizkaia",
    "Islas Baleares/Illes Balears", "Las Palmas", "Asturias", "Granada",
    "Pontevedra", "Santa Cruz de Tenerife", "Zaragoza", "Almería", "Córdoba",
    "Girona", "Guipúzcoa/Gipuzkoa", "Tarragona", "Toledo", "Badajoz",
    "Cantabria", "Castellón/Castelló", "Ciudad Real", "Huelva", "Jaén",
    "Navarra/Nafarroa", "Valladolid", "Álava/Araba", "Albacete", "Burgos",
    "Cáceres", "León", "Lleida", "Lugo", "Orense/Ourense", "La Rioja",
    "Salamanca", "Ávila", "Cuenca", "Guadalajara", "Huesca", "Palencia",
    "Segovia", "Teruel", "Zamora", "Soria", "Ceuta", "Melilla"
  ),
  nseats = c(
    37, 32, 16, 12, 12, 11, 10,  9,  8,  8,  8,  8,
    7,  7,  7,  7,  7,  6,  6,  6,  6,  6,  6,  5,
    5,  5,  5,  5,  5,  5,  5,  4,  4,  4,  4,  4,
    4,  4,  4,  4,  4,  3,  3,  3,  3,  3,  3,  3,
    3,  2,  1,  1
  ),
  id_elec = "02-2023-07-24")

# ----- join -----
total_seats_spain <-
  bind_rows(seats_1982, seats_1986, seats_1989, seats_1993,
            seats_1996, seats_2000, seats_2004, seats_2008,
            seats_2011, seats_2015, seats_2016, seats_2019a,
            seats_2019n, seats_2023) |>
  left_join(pollspain::cod_INE_prov_ccaa, by = "prov") |>
  mutate("id_INE_prov" = paste0(cod_INE_ccaa, "-", cod_INE_prov))

# total_seats_spain |> summarise("total_seats" = sum(nseats), .by = id_elec) |> filter(total_seats != 350)
# ----- use data -----

usethis::use_data(total_seats_spain, overwrite = TRUE,
                  compress = "xz")
