#' plot Bluefish July proportion indicators
#'
#' Plots proportion of central Atlantic colder than 18C,
#' between 18-25.6C, and warmer than 25.6C in July
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

plot_bf_july_proportion <- function(shadedRegion = NULL,
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
  load(here::here('offshore_habitat/data/july_proportion.rda'))
  
  
  # calculate mean and sd for reference lines on plots
  # not using sd for now but calculating in case we want to add later

  var.means <- july_proportion |>
    dplyr::group_by(Var) |>
    dplyr::mutate(mean = mean(Value, na.rm = TRUE)) |>
    dplyr::mutate(sd = sd(Value, na.rm = TRUE)) |>
    dplyr::mutate(upper = mean + sd) |>
    dplyr::mutate(lower = mean - sd) |>
    dplyr::ungroup() |> 
    dplyr::select(Var, mean, upper, lower) |>
    dplyr::distinct()
  
  # cold
  cold.mean <- var.means |>
    dplyr::filter(Var == "july_proportion_under_18C") |>
    dplyr::pull(mean)
  
  cold.lower <- var.means |>
    dplyr::filter(Var == "july_proportion_under_18C") |>
    dplyr::pull(lower)
  
  cold.upper <- var.means |>
    dplyr::filter(Var == "july_proportion_under_18C") |>
    dplyr::pull(upper)
  
  # medium
  medium.mean <- var.means |>
    dplyr::filter(Var == "july_proportion_18-25.6C") |>
    dplyr::pull(mean)
  
  medium.lower <- var.means |>
    dplyr::filter(Var == "july_proportion_18-25.6C") |>
    dplyr::pull(lower)
  
  medium.upper <- var.means |>
    dplyr::filter(Var == "july_proportion_18-25.6C") |>
    dplyr::pull(upper)
  
  # warm
  warm.mean <- var.means |>
    dplyr::filter(Var == "july_proportion_over_25.6C") |>
    dplyr::pull(mean)
  
  warm.lower <- var.means |>
    dplyr::filter(Var == "july_proportion_over_25.6C") |>
    dplyr::pull(lower)
  
  warm.upper <- var.means |>
    dplyr::filter(Var == "july_proportion_over_25.6C") |>
    dplyr::pull(upper)
  
  # plot cold
  p1 <- july_proportion |>
    dplyr::filter(Var == "july_proportion_under_18C") |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=cold.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Proportion of the central Atlantic colder than 18C in July") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # plot medium
  p2 <- july_proportion |>
    dplyr::filter(Var == "july_proportion_18-25.6C") |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=medium.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Proportion of the central Atlantic between 18-25.6C in July") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # plot warm
  p3 <- july_proportion |>
    dplyr::filter(Var == "july_proportion_over_25.6C") |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=warm.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Proportion of the central Atlantic warmer than 25.6C in July") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
    
  
  # combine plots
  
  p <- cowplot::plot_grid(p1,p2,p3, nrow=3,scale = 0.9) +
    cowplot::draw_label(expression("Proportion"), x=  0, y=0.5, vjust= 1.5, angle=90)
  
  return(p)
  
}
