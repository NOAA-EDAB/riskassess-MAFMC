#' plot Golden Tilefish mean bottom salinity
#'
#'Time series of annual mean bottom salinity at 78 m in GTF stat areas
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_gtf_bottom_sal <- function(shadedRegion = NULL,
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
  
  ribbon<- gtf_bottom_sal |>
    tidyr::pivot_wider(names_from = Var, values_from = Value)
  
  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- gtf_bottom_sal |>
    dplyr::filter(Var == 'annual_weighted_mean_sal') |>
    ggplot2::ggplot() +
    #Highlight last ten years
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Value))+
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Value))+
    ggplot2::geom_hline(yintercept=33,linetype=setup$hline.lty)+
    ggplot2::geom_hline(yintercept=36,linetype=setup$hline.lty)+
    ggplot2::geom_ribbon(data = ribbon, ggplot2::aes(ymin = conf.low , ymax = conf.high , x = Time), alpha = 0.2)+
    ggplot2::ggtitle("Annual average bottom salinity")+
    ggplot2::ylab("salinity (psu)")+
    ggplot2::xlab(ggplot2::element_blank())+
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Value)) +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  return(p)
}