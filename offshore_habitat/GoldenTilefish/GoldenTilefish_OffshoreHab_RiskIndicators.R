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

### load in data --------------------------------------------------------
data <- readxl::read_excel(here::here('offshore_habitat/Data/MAB_GoM_WGB_regAvg_TSanom_BSB_2025.xlsx'), sheet = 5)
# salinity is reading as a character for some reason. Fix that
data$S <- as.numeric(data$S)

### calculate annual mean temperature ----------------------------------
bottom.temp <- data |> 
  dplyr::mutate(date = lubridate::date_decimal(Year, tz = "America/New_York"))|> 
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |> 
  dplyr::group_by(year) |>
  dplyr::mutate(annual.mean.temperature = mean(T, na.rm = TRUE)) |>
  dplyr:: ungroup()

### plot annual mean temperature ----------------------------------------
# with horizontal lines at 9 and 14 degrees C
# which has been proposed as the thermal tolerance range for Golden Tilefish
# (Grimes and Turner, 1999; Nesslage et al., 2021; Steimle et al., 1999)
bottom.temp |> 
  ggplot2::ggplot(ggplot2::aes(year, annual.mean.temperature)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::geom_hline(yintercept = 9, linetype = "dashed") +
  ggplot2::geom_hline(yintercept = 14, linetype = "dashed") +
  ggplot2::labs(title = "Mean bottom temperature by year",
       x = "Year",
       y = expression("Mean bottom temperature ("*degree*"C)")) +
  ecodata::theme_ts()+
  ecodata::theme_title()

## Mean bottom salinity  ------------------------------------------------

### calculate annual mean salinity ---------------------------------------
bottom.sal <- data |> 
  dplyr::mutate(date = lubridate::date_decimal(Year, tz = "America/New_York"))|> 
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |> 
  dplyr::group_by(year) |>
  dplyr::mutate(annual.mean.salinity = mean(S, na.rm = TRUE)) |>
  dplyr:: ungroup()

## plot annual mean salinity ---------------------------------------
bottom.sal |> 
  ggplot2::ggplot(ggplot2::aes(year, annual.mean.salinity)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::labs(title = "Mean bottom salinity by year",
       x = "Year",
       y = expression("Mean bottom salinity (PSU)")) +
  ecodata::theme_ts()+
  ecodata::theme_title()

# Physical oceanography indicators ---------------------------------------

## Mean sea surface temperature  -----------------------------------------
ecodata::plot_long_term_sst()

## Shelf water volume, temp, salinity  -----------------------------------------

### load data --------------------------------------------------------
sw.data <- readxl::read_excel(here::here("offshore_habitat/Data/ShelfWaterVolume_BSB_update_2025.xlsx"))

### rename columns -------------------------------------------------
sw.names <- c('year.day','shw.t','shw.t.anom','shw.s','shw.s.anom','shw.v','shw.v.anom','year')
colnames(sw.data) <- sw.names

### plot shelf water volume by year ---------------------------------
sw.data |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = shw.v)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::labs(title = "Shelf water volume by year",
       x = "Year",
       y = expression("Shelf Water Volume (km"^3*")")) +
  ecodata::theme_ts()+
  ecodata::theme_title()

### plot shelf water temp by year ---------------------------------
sw.data |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = shw.t)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::labs(title = "Shelf water temperature by year",
       x = "Year",
       y = expression("Shelf Water Temperature ("*degree*"C)")) +
  ecodata::theme_ts()+
  ecodata::theme_title()

### plot shelf water salinity by year ---------------------------------
sw.data |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = shw.s)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::labs(title = "Shelf water salinity by year",
       x = "Year",
       y = expression("Shelf Water Salinity (PSU)")) +
  ecodata::theme_ts()+
  ecodata::theme_title()

## Gulf stream index  ----------------------------------------------------
ecodata::plot_gsi()
