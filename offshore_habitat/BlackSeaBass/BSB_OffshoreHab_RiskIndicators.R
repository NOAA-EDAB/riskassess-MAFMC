## Developing Offshore Habitat Risk Indicators for the    
## Mid Atlantic Risk Assessment. Indicators were informed 
## by the ESP                                             
##
## Author: M.T.Grezlik                                    
## Date: 01/30/2025                                       

# Load data ------------------------------------------------
# data_survive <- read_csv(here('offshore_habitat/Data/TSanom_MAB.csv'))
data_survive <- readxl::read_excel(here::here('offshore_habitat/Data/MAB_GoM_WGB_regAvg_TSanom_BSB_2025.xlsx'), sheet = 5)
# data_mix <- read_csv(here('offshore_habitat/Data/SHW_MAB.csv'))
data_mix <- readxl::read_excel(here::here('offshore_habitat/Data/ShelfWaterVolume_BSB_update_2025.xlsx'), sheet = 3)

## rename columns ------------------------------------------
mix_names <- c(year.day = 'Year Day', shelfwater.temp = 'ShW T',
                   shelfwater.temp.anom = 'ShW T anom', shelfwater.salinity = 'ShW S',
                   shelfwater.salinity.anom = 'ShW S anom', shelfwater.volume = 'ShW Vol',
                   shelfwater.volume.anom = 'ShW Vol anom')

data_mix <- dplyr::rename(data_mix, all_of(mix_names))




# Indicator: Overwinter survival --------------------------------
# Spatio-temporal variation in winter mean bottom temperature

# The ESP used mean winter (February and March) bottom temperatures calculated
# using the du Pontavice et al. (2023) method to average bottom temp estimates
# from different data sources.

winter <- c(2,3)

data_survive |> 
  dplyr::mutate(date = lubridate::date_decimal(data_survive$Year, tz = "America/New_York"))|> 
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |> 
  dplyr::filter(month %in% winter) |> 
  dplyr::group_by(year) |>
  dplyr::summarise(mean.winter.bottom.temp = mean(T, na.rm = TRUE)) |>
  dplyr::ungroup() |> 
  ggplot2::ggplot(ggplot2::aes(year,mean.winter.bottom.temp)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::labs(title = "Mean winter bottom temperature by year",
       x = "Year",
       y = expression("Mean winter bottom temperature ("*degree*"C)")) +
  ggplot2::geom_hline(yintercept = 6, linetype = "dashed") +
  ecodata::theme_ts()+
  ecodata::theme_title()



# Spatio-temporal variation in mean winter bottom temp is then calculated
# as the standard error of the mean of each year

data_survive |> 
  dplyr::mutate(date = lubridate::date_decimal(data_survive$Year, tz = "America/New_York"))|> 
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |> 
  dplyr::filter(month %in% winter) |> 
  dplyr::group_by(year) |>
  dplyr::mutate(mean.winter.bottom.temp = mean(T, na.rm = TRUE)) |>
  dplyr::mutate(st.var.mean.winter.bottom.temp = plotrix::std.error(T, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  ggplot2::ggplot(ggplot2::aes(year,st.var.mean.winter.bottom.temp)) +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Spatio-temporal variation in mean winter bottom temperature by year",
       x = "Year",
       y = expression("Standard error of mean winter bottom temperature ("*degree*"C)")) +
  ecodata::theme_ts()+
  ecodata::theme_title()



# Indicator: Overwinter mixing ------------------------
# High winter shelf water volume (>4000 km3) (Miller et al., 2016)

# Plot shelf water volume by year with dashed horizontal line at 4000 km3

# set winter months as Feb and March
winter <- c(2,3)

data_mix |> 
  dplyr::mutate(date = lubridate::date_decimal(data_mix$Year, tz = "America/New_York"))|> 
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |> 
  dplyr::filter(month %in% winter) |> 
  ggplot2::ggplot(ggplot2::aes(Year,shelfwater.volume)) +
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::ggtitle("Winter shelf water volume by year")+
  ggplot2::ylab(expression("Shelf water volume (km³)"))+
  ggplot2::geom_hline(yintercept = 4000, linetype = "dashed") +
  ecodata::theme_ts()+
  ecodata::theme_title()

# create average value for years with more than one winter volume
data_mix |> 
  dplyr::mutate(date = lubridate::date_decimal(data_mix$Year, tz = "America/New_York"))|> 
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |> 
  dplyr::filter(month %in% winter) |> 
  dplyr::group_by(year) |>
  dplyr::summarise(mean.winter.shelfwater.volume = mean(shelfwater.volume, na.rm = TRUE)) |>
  dplyr::ungroup() |> 
  ggplot2::ggplot(ggplot2::aes(year,mean.winter.shelfwater.volume)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(title = "Mean winter shelf water volume by year",
       x = "Year",
       y = expression("Mean winter shelf water volume (km³)")) +
  ggplot2::geom_hline(yintercept = 4000, linetype = "dashed") +
  ecodata::theme_ts()+
  ecodata::theme_title()

# Plot shelf water anomaly by year
data_mix |> 
  dplyr::mutate(date = lubridate::date_decimal(data_mix$Year, tz = "America/New_York"))|> 
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |> 
  dplyr::filter(month %in% winter) |> 
  ggplot2::ggplot(ggplot2::aes(Year,shelfwater.volume.anom)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::labs(title = "Winter Shelf Water Volume Anomaly by Year",
       x = "Year",
       y = "Shelf Water Volume Anomaly")
