#' plot mean shelf water volume
#'
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#'
#' @return ggplot object
#'
#' @export

plot_shw_vol <- function(shadedRegion=NULL,
                                report = "MidAtlantic") {
  
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }
  
  load(file = here::here('offshore_habitat/data/shw_vol_temp_sal.rda'))
  
  
  p <- shw_vol_temp_sal |>
    dplyr::filter(Var == 'shw.v') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::facet_wrap(~EPU) +
    ggplot2::ylab("Shelf water volume (km3)") +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle(paste(report,": mean shelf water volume")) +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  
  return(p)
}