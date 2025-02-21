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

### setup data --------------------------------------------------------
data.text <- RCurl::getURL("https://raw.githubusercontent.com/SSalois1/tilefish_indicators/refs/heads/main/Env%20Data/bt_ts_gtf_1970_2023.csv")
data <- read.csv(text = data.text)

gtf_bottom_temp <- data |> 
  dplyr::group_by(year) |> 
  dplyr::mutate(annual_weighted_mean_bt = mean(weighted_mean_bt, na.rm = TRUE)) |>
  dplyr::mutate(annual_sd_bt = sd(weighted_mean_bt, na.rm = TRUE)) |>
  dplyr::mutate(conf.low = annual_weighted_mean_bt - (2*annual_sd_bt)) |> 
  dplyr::mutate(conf.high = annual_weighted_mean_bt + (2*annual_sd_bt)) |>
  dplyr:: ungroup() |> 
  dplyr:: select(year, annual_weighted_mean_bt, conf.low, conf.high) |> 
  dplyr:: distinct() |> 
  tidyr::pivot_longer(cols = 2:4, names_to = 'Var', values_to = 'Value') |> 
  dplyr:: rename(Time = year) |> 
  dplyr:: mutate(EPU = 'MAB')

### save data as .rda ------------------------------------------------
save(gtf_bottom_temp, file = here::here('offshore_habitat/data/gtf_bottom_temp.rda'))

### plot using function ---------------------------------------

source(here::here('offshore_habitat/R/plot_gtf_bottom_temp.R'))
plot_gtf_bottom_temp()


## Mean bottom salinity  ------------------------------------------------

### setup data --------------------------------------------------------
data.text <- RCurl::getURL("https://raw.githubusercontent.com/SSalois1/tilefish_indicators/refs/heads/main/Env%20Data/sal_78m_monthly_ts_gtf.csv")
data <- read.csv(text = data.text)

# Past 2019, there are 366 observations in the data set
# I am assuming this is the data for 2020 (which was a leap year)
# Double check this assumption <- dplyr::filter(data, is.na(month))
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


## plot annual mean salinity ---------------------------------------

gtf_bottom_sal <- data |> 
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
save(gtf_bottom_sal, file = here::here('offshore_habitat/data/gtf_bottom_sal.rda'))
  
### plot using function ---------------------------------------
source(here::here('offshore_habitat/R/plot_gtf_bottom_sal.R'))
plot_gtf_bottom_sal()


  ggplot2::ggplot(ggplot2::aes(year, annual_weighted_mean_sal, ymin = conf.low, ymax = conf.high)) +
  ggplot2::geom_line() +
  ggplot2::geom_ribbon(alpha = 0.2) +
  ggplot2::labs(title = "Mean bottom salinity by year",
                x = "Year",
                y = expression("Mean bottom salinity (psu)")) +
  ggplot2::geom_hline(yintercept = 33, linetype = "dashed") +
  ggplot2::geom_hline(yintercept = 36, linetype = "dashed") +
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
  ggplot2::labs(title = "Long-term shelf water volume",
       x = "Year",
       y = expression("Shelf Water Volume (km"^3*")")) +
  ecodata::theme_ts()+
  ecodata::theme_title()

sw.data |>
  dplyr::mutate(date = lubridate::date_decimal(sw.data$year, tz = "America/New_York"))|> 
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |>
  dplyr::group_by(year) |>
  dplyr::summarise(mean_shw_v = mean(shw.v, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = mean_shw_v)) +
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
  ggplot2::labs(title = "Long-term shelf water temperature",
                x = "Year",
                y = expression("Shelf Water Temperature ("*degree*"C)")) +
  ecodata::theme_ts()+
  ecodata::theme_title()

sw.data |>
  dplyr::mutate(date = lubridate::date_decimal(sw.data$year, tz = "America/New_York"))|> 
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |>
  dplyr::group_by(year) |>
  dplyr::summarise(mean_shw_t = mean(shw.t, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = mean_shw_t)) +
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
  ggplot2::labs(title = "Long-term shelf water salinity",
       x = "Year",
       y = expression("Shelf Water Salinity (PSU)")) +
  ecodata::theme_ts()+
  ecodata::theme_title()

sw.data |>
  dplyr::mutate(date = lubridate::date_decimal(sw.data$year, tz = "America/New_York"))|> 
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date)) |>
  dplyr::group_by(year) |>
  dplyr::summarise(mean_shw_s = mean(shw.s, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = mean_shw_s)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::labs(title = "Shelf water salinity by year",
                x = "Year",
                y = expression("Shelf Water Salinity (PSU)")) +
  ecodata::theme_ts()+
  ecodata::theme_title()

## Gulf stream index  ----------------------------------------------------
ecodata::plot_gsi()
