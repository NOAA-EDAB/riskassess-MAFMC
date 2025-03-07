#' plot mean shelf water salinity
#'
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#'
#' @return ggplot object
#'
#' @export

plot_shw_sal <- function(shadedRegion=NULL,
                          report = "MidAtlantic") {
  
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }
  
  load(file = here::here('offshore_habitat/data/shw_vol_temp_sal.rda'))
  
  # calculate mean and sd for reference lines on plots
  # not using sd for now but calculating in case we want to add later
  var.means <- shw_vol_temp_sal |>
    dplyr::group_by(Var) |>
    dplyr::mutate(mean = mean(Value, na.rm = TRUE)) |>
    dplyr::mutate(sd = sd(Value, na.rm = TRUE)) |>
    dplyr::mutate(upper = mean + sd) |>
    dplyr::mutate(lower = mean - sd) |>
    dplyr::ungroup() |> 
    dplyr::select(Var, mean, upper, lower) |>
    dplyr::distinct()
  
  shw_sal_mean <-  var.means |>
    dplyr::filter(Var == 'shw.s') |>
    dplyr::pull(mean)
  
  p <- shw_vol_temp_sal |>
    dplyr::filter(Var == 'shw.s') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=shw_sal_mean,linetype=setup$hline.lty)+
    ggplot2::facet_wrap(~EPU) +
    ggplot2::ylab("Salinity (psu)") +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle(paste(report,": mean shelf water salinity")) +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  
  return(p)
}
