# Recreating the NMFS bottom trawl pull from 2022 Bluefish ESP

# Copying Abbys code from:
# https://github.com/atyrell3/RT2022/blob/main/bluefishESP/data-raw/scripts/pull_nmfs_bottom_trawl.R
# because sourcing didn't work

channel <- dbutils::connect_to_database("NEFSC_USERS"," MGREZLIK")

# pull all trawl data
pull <- survdat::get_survdat_data(channel,
                                  all.season = TRUE,
                                  getBio = FALSE)

bluefish_nmfs <- pull$survdat %>%
  dplyr::filter(SVSPP == 135)

usethis::use_data(bluefish_nmfs, overwrite = TRUE)

all_nmfs <- pull$survdat %>%
  dplyr::select(-c(SVSPP, ABUNDANCE, BIOMASS, LENGTH, NUMLEN)) %>%
  dplyr::distinct()
usethis::use_data(all_nmfs, overwrite = TRUE)

# pull biological trawl data
`%like%` <- data.table::`%like%`
pull <- survdat::get_survdat_data(channel,
                                  all.season = TRUE,
                                  getBio = TRUE)

bluefish_nmfs_bio <- pull$survdat %>%
  dplyr::filter(SVSPP == 135)

usethis::use_data(bluefish_nmfs_bio, overwrite = TRUE)
