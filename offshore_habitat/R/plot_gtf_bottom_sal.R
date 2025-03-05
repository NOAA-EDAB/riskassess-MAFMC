#' plot Golden Tilefish bottom salinity
#'
#'Time series of weighted mean salinity in GTF stat areas
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_gtf_sal <- function(shadedRegion = NULL,
                                 report="MidAtlantic") {
  
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)
  
  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }
  
  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  
  load(file = here::here('offshore_habitat/data/gtf_bottom_sal.rda'))
  
  # Changing Time from decimal year to integer of months since
  # the start of the time series so the geom_gls function can be used.
  # Year is retained for labeling purposes.
  test <- gtf_bottom_sal |> 
            dplyr::filter(Var == 'weighted_mean_sal_78m') |>
            dplyr::mutate(Year = Time) |> 
            dplyr::mutate(Time = 1:length(Var))
  
  # Create table for X axis label
  lab.data <- test |>
                dplyr::filter(Year == floor(Year)) |> 
                dplyr::filter(Year %in% c(2000,2010,2020))
  
  # Create ribbon data for confidence interval
  ribbon<- gtf_bottom_sal |>
    tidyr::pivot_wider(names_from = Var, values_from = Value) |> 
    dplyr::mutate(Time = 1:length(Units))
  
  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- test |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Value))+
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Value))+
    ggplot2::scale_x_continuous(
      breaks = lab.data$Time,    
      labels = lab.data$Year
      ) +
    ggplot2::geom_hline(yintercept=33,linetype=setup$hline.lty)+
    ggplot2::geom_hline(yintercept=36,linetype=setup$hline.lty)+
    ggplot2::geom_ribbon(data = ribbon, ggplot2::aes(ymin = conf.low , ymax = conf.high , x = Time), alpha = 0.2)+
    ggplot2::ggtitle("Weighted mean salinity")+
    ggplot2::ylab("Degree C")+
    ggplot2::xlab(ggplot2::element_blank())+
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Value)) +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  return(p)
}
