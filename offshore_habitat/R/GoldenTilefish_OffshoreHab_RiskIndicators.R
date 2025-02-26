## Developing Offshore Habitat Risk Indicators for the    
## Mid Atlantic Risk Assessment. Indicators were informed 
## by the ESP                                             
##
## Author: M.T.Grezlik                                    
## Date: 01/30/2025  


# ESP had 8 indicators for Golden Tilefish, 5 Habitat condition indicators
# and 3 Physical oceanography indicators. The indicators align with the
# SOE report.

# Habitat condition indicators -------------------------------------------

## Cold pool index, spatial extent index, and persistence index  ---------
ecodata::plot_cold_pool()

## Mean bottom temperature  ----------------------------------------------

## setup data --------------------------------------------------------
# data.text <- RCurl::getURL("https://raw.githubusercontent.com/SSalois1/tilefish_indicators/refs/heads/main/Env%20Data/bt_ts_gtf_1970_2023.csv")
# data <- read.csv(text = data.text)
# 
# annual_avg_gtf_bottom_temp <- data |>
#   dplyr::group_by(year) |>
#   dplyr::mutate(annual_weighted_mean_bt = mean(weighted_mean_bt, na.rm = TRUE)) |>
#   dplyr::mutate(annual_sd_bt = sd(weighted_mean_bt, na.rm = TRUE)) |>
#   dplyr::mutate(conf.low = annual_weighted_mean_bt - (2*annual_sd_bt)) |>
#   dplyr::mutate(conf.high = annual_weighted_mean_bt + (2*annual_sd_bt)) |>
#   dplyr:: ungroup() |>
#   dplyr:: select(year, annual_weighted_mean_bt, conf.low, conf.high) |>
#   dplyr:: distinct() |>
#   tidyr::pivot_longer(cols = 2:4, names_to = 'Var', values_to = 'Value') |>
#   dplyr:: rename(Time = year) |>
#   dplyr:: mutate(EPU = 'MAB')
# 
# 
# gtf_bottom_temp <- data |>
#   dplyr::mutate(day = 1) |>
#   dplyr::mutate(date = as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")) |>
#   dplyr::mutate(date = lubridate::decimal_date(date)) |>
#   dplyr::rename(Time = date) |>
#   dplyr::mutate(conf.low = weighted_mean_bt - (2*sd_bt)) |>
#   dplyr::mutate(conf.high = weighted_mean_bt + (2*sd_bt)) |>
#   dplyr::select(Time, weighted_mean_bt, sd_bt, conf.low, conf.high) |>
#   tidyr::pivot_longer(cols = 2:5, names_to = 'Var', values_to = 'Value') |>
#   dplyr::mutate(EPU = 'MAB') |>
#   dplyr::mutate(Units = 'degreeC') |>
#   dplyr::select(Time, Value, Var, EPU, Units)

### save data as .rda ------------------------------------------------
# save(annual_avg_gtf_bottom_temp, file = here::here('offshore_habitat/data/annual_avg_gtf_bottom_temp.rda'))
# save(gtf_bottom_temp, file = here::here('offshore_habitat/data/gtf_bottom_temp.rda'))

### plot using function ---------------------------------------

source(here::here('offshore_habitat/R/plot_gtf_bottom_temp.R'))
plot_gtf_bottom_temp()

source(here::here('offshore_habitat/R/plot_annual_avg_gtf_bottom_temp.R'))
plot_annual_avg_gtf_bottom_temp()



## Mean bottom salinity  ------------------------------------------------

### setup data --------------------------------------------------------
# data.text <- RCurl::getURL("https://raw.githubusercontent.com/SSalois1/tilefish_indicators/refs/heads/main/Env%20Data/sal_78m_monthly_ts_gtf.csv")
# data <- read.csv(text = data.text)

# Past 2019, there are 366 observations in the data set
# I am assuming this is the data for 2020 (which was a leap year)
# Double check this assumption
missing.data <- data |>
  dplyr::filter(is.na(month)) |>
  dplyr::mutate(year = stringr::str_replace(year,"dd_sal_78_2020_2022_", "")) |>
  dplyr::mutate(julian.day = as.numeric(year)) |>
  dplyr::mutate(year = "2020") |>
  dplyr::mutate(date = as.Date(paste(year, julian.day, sep = "-"), format = "%Y-%j")) |>
  dplyr::mutate(month = as.integer(format(date, "%m"))) |>
  dplyr::select(-julian.day, -date) |>
  dplyr::group_by(year,month) |>
  dplyr::summarise(dplyr::across(everything(), mean, na.rm = TRUE)) |>
  dplyr::ungroup()

# remove 2020 data from full data set
data <- data |>
  dplyr::filter(!is.na(month))

# bind rows of data and missing.data
data <- rbind(data, missing.data)


  gtf_bottom_sal <- data |>
    dplyr::mutate(day = 1) |>
    dplyr::mutate(date = as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")) |>
    dplyr::mutate(date = lubridate::decimal_date(date)) |>
    dplyr::rename(Time = date) |>
    dplyr::mutate(conf.low = weighted_mean_sal_78m - (2*sd_sal_78m)) |>
    dplyr::mutate(conf.high = weighted_mean_sal_78m + (2*sd_sal_78m)) |>
    dplyr::select(Time, weighted_mean_sal_78m, sd_sal_78m, conf.low, conf.high) |>
    tidyr::pivot_longer(cols = 2:5, names_to = 'Var', values_to = 'Value') |>
    dplyr::mutate(EPU = 'MAB') |>
    dplyr::mutate(Units = 'degreeC') |>
    dplyr::select(Time, Value, Var, EPU, Units)


annual_avg_gtf_bottom_sal <- data |>
  dplyr::group_by(year) |>
  dplyr::mutate(annual_weighted_mean_sal = mean(weighted_mean_sal_78m, na.rm = TRUE)) |>
  dplyr::mutate(annual_sd_sal = sd(weighted_mean_sal_78m, na.rm = TRUE)) |>
  dplyr::mutate(conf.low = annual_weighted_mean_sal - (2*annual_sd_sal)) |>
  dplyr::mutate(conf.high = annual_weighted_mean_sal + (2*annual_sd_sal)) |>
  dplyr:: ungroup() |>
  dplyr:: select(year, annual_weighted_mean_sal, conf.low, conf.high) |>
  dplyr:: distinct() |>
  tidyr::pivot_longer(cols = 2:4, names_to = 'Var', values_to = 'Value') |>
  dplyr:: rename(Time = year) |>
  dplyr:: mutate(Time = as.integer(Time)) |>
  dplyr:: mutate(EPU = 'MAB')

### save data as .rda ------------------------------------------------

# save(gtf_bottom_sal, file = here::here('offshore_habitat/data/gtf_bottom_sal.rda'))
# 
# save(annual_avg_gtf_bottom_sal, file = here::here('offshore_habitat/data/annual_avg_gtf_bottom_sal.rda'))
  
### plot using function ---------------------------------------
source(here::here('offshore_habitat/R/plot_gtf_bottom_sal.R'))
plot_gtf_sal()

source(here::here('offshore_habitat/R/plot_annual_avg_gtf_bottom_sal.R'))
plot_annual_avg_gtf_bottom_sal()


# Physical oceanography indicators ---------------------------------------

## Mean sea surface temperature  -----------------------------------------
ecodata::plot_long_term_sst()

## Shelf water  -----------------------------------------

### volume -------------------------------------------------
#### setup data --------------------------------------------------------
# sw.data <- readxl::read_excel(here::here("offshore_habitat/data-raw/ShelfWaterVolume_BSB_update_2025.xlsx"))

# # rename columns
# sw.names <- c('year.day','shw.t','shw.t.anom','shw.s','shw.s.anom','shw.v','shw.v.anom','year')
# colnames(sw.data) <- sw.names

# deal with date, average by year, and format
shw_vol_temp_sal <- sw.data |>
  dplyr::mutate(date = lubridate::date_decimal(sw.data$year, tz = "America/New_York"))|>
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |>
  dplyr::select(year,shw.t, shw.t.anom, shw.s, shw.s.anom, shw.v, shw.v.anom) |>
  dplyr::group_by(year) |>
  dplyr::summarise(dplyr::across(everything(), mean, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  tidyr::pivot_longer(cols = 2:7, names_to = 'Var', values_to = 'Value') |>
  dplyr::rename(Time = year) |>
  dplyr::mutate(EPU = 'MAB')

#### save data as .rda ------------------------------------------------
# save(shw_vol_temp_sal, file = here::here('offshore_habitat/data/shw_vol_temp_sal.rda'))

#### plot using function ---------------------------------------
source(here::here('offshore_habitat/R/plot_shw_vol.R'))
plot_shw_vol()

#### alternative plots -------------------------------------------------

# # Long-term shelf water volume
# sw.data <- readxl::read_excel(here::here("offshore_habitat/data-raw/ShelfWaterVolume_BSB_update_2025.xlsx"))
# sw.data |>
#   ggplot2::ggplot(ggplot2::aes(x = year, y = shw.v)) +
#   ggplot2::geom_point() +
#   ggplot2::geom_smooth(method = "lm") +
#   ggplot2::labs(title = "Long-term shelf water volume",
#        x = "Year",
#        y = expression("Shelf Water Volume (km"^3*")")) +
#   ecodata::theme_ts()+
#   ecodata::theme_title()
# 
# sw.data |>
#   dplyr::mutate(date = lubridate::date_decimal(sw.data$year, tz = "America/New_York"))|> 
#   dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |>
#   dplyr::group_by(year) |>
#   dplyr::summarise(mean_shw_v = mean(shw.v, na.rm = TRUE)) |>
#   dplyr::ungroup() |>
#   ggplot2::ggplot(ggplot2::aes(x = year, y = mean_shw_v)) +
#   ggplot2::geom_point() +
#   ggplot2::geom_smooth(method = "lm") +
#   ggplot2::labs(title = "Shelf water volume by year",
#                 x = "Year",
#                 y = expression("Shelf Water Volume (km"^3*")")) +
#   ecodata::theme_ts()+
#   ecodata::theme_title()

### temperature -----------------------------------------------
#### plot using function ---------------------------------

source(here::here('offshore_habitat/R/plot_shw_temp.R'))
plot_shw_temp()


#### alternative plots -------------------------------------------------

# # Long-term shelf water temperature
# sw.data <- readxl::read_excel(here::here("offshore_habitat/data-raw/ShelfWaterVolume_BSB_update_2025.xlsx"))
# 
# sw.data |>
#   ggplot2::ggplot(ggplot2::aes(x = year, y = shw.t)) +
#   ggplot2::geom_point() +
#   ggplot2::geom_smooth(method = "lm") +
#   ggplot2::labs(title = "Long-term shelf water temperature",
#                 x = "Year",
#                 y = expression("Shelf Water Temperature ("*degree*"C)")) +
#   ecodata::theme_ts()+
#   ecodata::theme_title()


### salinity -------------------------------------------
#### plot using function ---------------------------------

source(here::here('offshore_habitat/R/plot_shw_sal.R'))
plot_shw_sal()



# #### alternative plots -------------------------------------------------
# sw.data <- readxl::read_excel(here::here("offshore_habitat/data-raw/ShelfWaterVolume_BSB_update_2025.xlsx"))
# 
# sw.data |>
#   ggplot2::ggplot(ggplot2::aes(x = year, y = shw.s)) +
#   ggplot2::geom_point() +
#   ggplot2::geom_smooth(method = "lm") +
#   ggplot2::labs(title = "Long-term shelf water salinity",
#        x = "Year",
#        y = expression("Shelf Water Salinity (PSU)")) +
#   ecodata::theme_ts()+
#   ecodata::theme_title()


## Gulf stream index  ----------------------------------------------------
ecodata::plot_gsi()
