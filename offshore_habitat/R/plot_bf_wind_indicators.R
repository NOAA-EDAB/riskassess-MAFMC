#' plot Bluefish wind indicators
#'
#' Plots mean crosshore and alongshore wind in central Atlantic in April and May
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_bf_wind_indicators <- function(shadedRegion = NULL,
                                    report="MidAtlantic",
                                    EPU="MAB") {
  
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)
  
  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(EPU %in% c("GB","GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- EPU
  }
  
  # load data
  load(here::here('offshore_habitat/data/wind_indicators.rda')) 
  
  # calculate mean and sd for reference lines on plots
  # not using sd for now but calculating in case we want to add later
  
  var.means <- wind_indicators |>
    dplyr::group_by(Var) |>
    dplyr::mutate(mean = mean(Value, na.rm = TRUE)) |>
    dplyr::mutate(sd = sd(Value, na.rm = TRUE)) |>
    dplyr::mutate(upper = mean + sd) |>
    dplyr::mutate(lower = mean - sd) |>
    dplyr::ungroup() |> 
    dplyr::select(Var, mean, upper, lower) |>
    dplyr::distinct()
  
  
  # cross shore
  crossshore.mean <- var.means |>
    dplyr::filter(Var == "crossshore_wind") |>
    dplyr::pull(mean)
  
  crossshore.lower <- var.means |>
    dplyr::filter(Var == "crossshore_wind") |>
    dplyr::pull(lower)
  
  crossshore.upper <- var.means |>
    dplyr::filter(Var == "crossshore_wind") |>
    dplyr::pull(upper)
  
  # along shore
  alongshore.mean <- var.means |>
    dplyr::filter(Var == "longshore_wind") |>
    dplyr::pull(mean)
  
  alongshore.lower <- var.means |>
    dplyr::filter(Var == "longshore_wind") |>
    dplyr::pull(lower)
  
  alongshore.upper <- var.means |>
    dplyr::filter(Var == "longshore_wind") |>
    dplyr::pull(upper)
  
  # plot cross shore
  
  p1 <- wind_indicators |>
    dplyr::filter(Var == "crossshore_wind") |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=crossshore.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Mean cross shore wind (m/s) in the central Atlantic in April and May") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # plot along shore
  
  p2 <- wind_indicators |>
    dplyr::filter(Var == "longshore_wind") |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=alongshore.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Mean along shore wind (m/s) in the central Atlantic in April and May") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # combine plots
  
  p <- cowplot::plot_grid(p1,p2, nrow=2,scale = 0.9)
  
  return(p)
  
}