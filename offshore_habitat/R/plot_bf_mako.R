#' plot Bluefish Mako Shark B/Bmsy indicator
#'
#' Plots shortfin mako shark B/Bmsy in the North Atlantic
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

plot_bf_mako <- function(shadedRegion = NULL,
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
  load(here::here('offshore_habitat/data/mako.rda'))
  
  # calculate mean and sd for reference lines on plots
  # not using sd for now but calculating in case we want to add later
  
  var.means <- mako |>
    dplyr::group_by(Var) |>
    dplyr::mutate(mean = mean(Value, na.rm = TRUE)) |>
    dplyr::mutate(sd = sd(Value, na.rm = TRUE)) |>
    dplyr::mutate(upper = mean + sd) |>
    dplyr::mutate(lower = mean - sd) |>
    dplyr::ungroup() |> 
    dplyr::select(Var, mean, upper, lower) |>
    dplyr::distinct()
  
  mako.mean <- var.means |>
    dplyr::pull(mean)
  
  mako.lower <- var.means |>
    dplyr::pull(lower)
  
  mako.upper <- var.means |>
    dplyr::pull(upper)
  
  # plot mako B/Bmsy
  p <- mako |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=1.0,linetype=setup$hline.lty)+
    ggplot2::ylim(0,2) +
    ggplot2::ylab("B/Bmsy") +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Shortfin mako shark B/Bmsy in the North Atlantic") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  return(p)
  
}
