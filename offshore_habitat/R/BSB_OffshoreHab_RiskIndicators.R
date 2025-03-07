## Developing Offshore Habitat Risk Indicators for the    
## Mid Atlantic Risk Assessment. Indicators were informed 
## by the ESP                                             
##
## Author: M.T.Grezlik                                    
## Date: 01/30/2025                                       

# Indicator: Overwinter survival --------------------------------
## Mean bottom temperature   ------------------------------------

# The ESP used mean winter (February and March) bottom temperatures calculated
# using the du Pontavice et al. (2023) method to average bottom temp estimates
# from different data sources.

### setup data ------------------------------------------------------
# data_survive <- readxl::read_excel(here::here('offshore_habitat/data-raw/MAB_GoM_WGB_regAvg_TSanom_BSB_2025.xlsx'), sheet = 5)
# 
# winter <- c(2,3)
# 
# winter_bottom_temp <- data_survive |>
#   dplyr::mutate(date = lubridate::date_decimal(data_survive$Year, tz = "America/New_York"))|>
#   dplyr::mutate(Time = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |>
#   dplyr::filter(month %in% winter) |>
#   dplyr::group_by(Time) |>
#   dplyr::summarise(Value = mean(T, na.rm = TRUE)) |>
#   dplyr::ungroup() |>
#   dplyr::mutate(Var = 'Mean winter bottom temperature') |>
#   dplyr::mutate(EPU = 'MAB') |>
#   dplyr::mutate(Units = 'degreesC')
# 
### save data as .rda -------------------------------------------
# save(winter_bottom_temp, file = here::here('offshore_habitat/data/winter_bottom_temp.rda'))

### plot using function -------------------------------------------
source(here::here('offshore_habitat/R/plot_winter_bottom_temp.R'))
plot_winter_bottom_temp()

### alternative plot ---------------------------------------------
# Could also use SOE plot, but this is annual average bottom temp
# not winter bottom temp as was presented to the WG
ecodata::plot_bottom_temp()


## Spatio-temporal variation in mean winter bottom temperature ---------
# calculated as the standard error of the mean of each year

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ###
## Many years have 1 or 0 winter observations per year ##
## so cannot calculate a standard error of the mean.   ##
## Leaving for now.                                    ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ###



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
## Shelf water volume ----------------------------------
# High winter shelf water volume (>4000 km3) (Miller et al., 2016)

### setup data ------------------------------------------------------
# data_mix <- readxl::read_excel(here::here('offshore_habitat/data-raw/ShelfWaterVolume_BSB_update_2025.xlsx'), sheet = 3)
# 
# # rename columns
# mix_names <- c(year.day = 'Year Day', shelfwater.temp = 'ShW T',
#                shelfwater.temp.anom = 'ShW T anom', shelfwater.salinity = 'ShW S',
#                shelfwater.salinity.anom = 'ShW S anom', shelfwater.volume = 'ShW Vol',
#                shelfwater.volume.anom = 'ShW Vol anom')
# 
# data_mix <- dplyr::rename(data_mix, all_of(mix_names))
# 
# # set winter months as Feb and March
# winter <- c(2,3)
# 
# 
# # create average value for years with more than one winter volume
# winter_shw_vol <- data_mix |> 
#   dplyr::mutate(date = lubridate::date_decimal(data_mix$Year, tz = "America/New_York"))|> 
#   dplyr::mutate(Time = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |> 
#   dplyr::filter(month %in% winter) |> 
#   dplyr::group_by(Time) |>
#   dplyr::summarise(Value = mean(shelfwater.volume, na.rm = TRUE)) |>
#   dplyr::ungroup() |> 
#   dplyr::mutate(Var = 'Mean winter shelf water volume') |>
#   dplyr::mutate(EPU = 'MAB') |>
#   dplyr::mutate(Units = 'km3')
#   
### save data as .rda -------------------------------------------
# save(winter_shw_vol, file = here::here('offshore_habitat/data/winter_shw_vol.rda'))


### plot using function -------------------------------------------
source(here::here('offshore_habitat/R/plot_winter_shw_vol.R'))
plot_winter_shw_vol()



### alternative plots ---------------------------------------------

# plot all winter shelf water data, not annual averages
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