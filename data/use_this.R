library(httr)  # Ensure httr is loaded
library(readxl)

# Define URLs for the files
urls <- list(
  cod_INE_mun = "https://github.com/mikadsr/Pollspain-data/raw/main/get%20auxiliary%20data/cod_INE_mun.rda",
  dates_elections_spain = "https://github.com/mikadsr/Pollspain-data/raw/main/get%20auxiliary%20data/dates_elections_spain.rda",
  party_colors_hex = "https://github.com/mikadsr/Pollspain-data/raw/main/get%20auxiliary%20data/party_colors_hex.rda",
  seat_distribution_congress = "https://github.com/mikadsr/Pollspain-data/raw/main/get%20auxiliary%20data/seat_distribution_congress.rda"
)

# Function to download and load .rda files
load_rda_file <- function(url, file_name) {
  local_file <- tempfile(fileext = ".rda")
  httr::GET(url, httr::write_disk(local_file))
  load(local_file, envir = .GlobalEnv)
  unlink(local_file)
}

# Load .rda files
for (file in names(urls)) {
  if (grepl("\\.rda$", urls[[file]])) {
    load_rda_file(urls[[file]], file)
  }
}

# Verify the objects are loaded

library(usethis)

# Save the loaded .rda files to the /data directory
usethis::use_data(cod_INE_mun, overwrite = TRUE)
usethis::use_data(dates_elections_spain, overwrite = TRUE)
usethis::use_data(party_colors_hex, overwrite = TRUE)
usethis::use_data(seat_distribution_congress, overwrite = TRUE)
