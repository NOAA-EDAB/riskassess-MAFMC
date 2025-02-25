#' plot Bluefish Climate indicators
#'
#' Plots first, last, and number of days when central Atlantic
#' is warmer than 18C
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

plot_bf_climate <- function(shadedRegion = NULL,
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
  load(file = here::here('offshore_habitat/data/first_18_day.rda'))
  load(file = here::here('offshore_habitat/data/last_18_day.rda'))
  load(file = here::here('offshore_habitat/data/n_18_day.rda'))
  
  
# calculate mean and sd for reference lines on plots
# not using sd for now but calculating in case we want to add later
  first.var.means <- first_18_day |>
    dplyr::group_by(Var) |>
    dplyr::mutate(mean = mean(Value, na.rm = TRUE)) |>
    dplyr::mutate(sd = sd(Value, na.rm = TRUE)) |>
    dplyr::mutate(upper = mean + sd) |>
    dplyr::mutate(lower = mean - sd) |>
    dplyr::ungroup() |> 
    dplyr::select(Var, mean, upper, lower) |>
    dplyr::distinct()

  last.var.means <- last_18_day |>
    dplyr::group_by(Var) |>
    dplyr::mutate(mean = mean(Value, na.rm = TRUE)) |>
    dplyr::mutate(sd = sd(Value, na.rm = TRUE)) |>
    dplyr::mutate(upper = mean + sd) |>
    dplyr::mutate(lower = mean - sd) |>
    dplyr::ungroup() |> 
    dplyr::select(Var, mean, upper, lower) |>
    dplyr::distinct()
  
  n.var.means <- n_18_day |>
    dplyr::group_by(Var) |>
    dplyr::mutate(mean = mean(Value, na.rm = TRUE)) |>
    dplyr::mutate(sd = sd(Value, na.rm = TRUE)) |>
    dplyr::mutate(upper = mean + sd) |>
    dplyr::mutate(lower = mean - sd) |>
    dplyr::ungroup() |> 
    dplyr::select(Var, mean, upper, lower) |>
    dplyr::distinct()
  
first.mean <- first.var.means |>
  dplyr::pull(mean)

last.mean <- last.var.means |>
  dplyr::pull(mean)

n.mean <- n.var.means |>
  dplyr::pull(mean)
  

# generate plots
p1 <-  first_18_day |>
  ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
  ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                    xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                    ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line() +
  ggplot2::geom_point()  +
  ggplot2::geom_hline(yintercept=first.mean,linetype=setup$hline.lty)+
  ggplot2::ylab("Day of Year") +
  ggplot2::xlab(ggplot2::element_blank())+
  ggplot2::ggtitle("First day of the year when the mean temperature of the central Atlantic region is warmer than 18C") +
  ecodata::geom_gls() +
  ecodata::theme_ts()+
  ecodata::theme_facet()+
  ecodata::theme_title()

p2 <-  last_18_day |>
  ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
  ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                    xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                    ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line() +
  ggplot2::geom_point()  +
  ggplot2::geom_hline(yintercept=last.mean,linetype=setup$hline.lty)+
  ggplot2::ylab("Day of Year") +
  ggplot2::xlab(ggplot2::element_blank())+
  ggplot2::ggtitle("Last day of the year when the mean temperature of the central Atlantic region is warmer than 18C") +
  ecodata::geom_gls() +
  ecodata::theme_ts()+
  ecodata::theme_facet()+
  ecodata::theme_title()

p3 <-  n_18_day |>
  ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
  ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                    xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                    ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line() +
  ggplot2::geom_point()  +
  ggplot2::geom_hline(yintercept=n.mean,linetype=setup$hline.lty)+
  ggplot2::ylab("Number of days") +
  ggplot2::xlab("Year")+
  ggplot2::ggtitle("Number of days when the mean temperature of the central Atlantic region is warmer than 18C") +
  ecodata::geom_gls() +
  ecodata::theme_ts()+
  ecodata::theme_facet()+
  ecodata::theme_title()

# combine plots

p <- cowplot::plot_grid(p1,p2,p3, nrow=3,scale = 0.9)

return(p)
  
}