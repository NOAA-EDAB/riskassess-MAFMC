#' plot Bluefish Center of Gravity
#'
#' Plots faceted northings and eastings by size class
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

plot_bf_cog <- function(shadedRegion = NULL,
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
  
  
load(file = here::here('offshore_habitat/data/bf_cog.rda'))
  
center_of_gravity$Var <- factor(center_of_gravity$Var,levels = c("small eastings",
                                               "small northings",
                                               "medium eastings",
                                               "medium northings",
                                               "large eastings",
                                               "large northings"))

var.means <- center_of_gravity |>
  dplyr::group_by(Var) |>
  dplyr::mutate(mean = mean(Value, na.rm = TRUE)) |>
  dplyr::mutate(sd = sd(Value, na.rm = TRUE)) |>
  dplyr::mutate(upper = mean + sd) |>
  dplyr::mutate(lower = mean - sd) |>
  dplyr::ungroup() |> 
  dplyr::select(Var, mean, upper, lower) |>
  dplyr::distinct()
  
  
  
# small eastings 
 small.mean <- var.means |>
    dplyr::filter(Var == 'small eastings') |>
    dplyr::pull(mean)
  
  small.lower <- var.means |>
    dplyr::filter(Var == 'small eastings') |>
    dplyr::pull(lower)
  
  small.upper <- var.means |>
    dplyr::filter(Var == 'small eastings') |>
    dplyr::pull(upper)
  
# plot small eastings
  
 p1 <-  center_of_gravity |>
    dplyr::filter(Var == 'small eastings') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=small.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Small (<= 30.3 cm) Eastings") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
# small northings  
  small.mean <- var.means |>
    dplyr::filter(Var == 'small northings') |>
    dplyr::pull(mean)
  
  small.lower <- var.means |>
    dplyr::filter(Var == 'small northings') |>
    dplyr::pull(lower)
  
  small.upper <- var.means |>
    dplyr::filter(Var == 'small northings') |>
    dplyr::pull(upper)
  
# plot small northings
  
p2 <- center_of_gravity |>
    dplyr::filter(Var == 'small northings') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=small.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Small (<= 30.3 cm) Northings") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  
  # medium eastings  
medium.mean <- var.means |>
    dplyr::filter(Var == 'medium eastings') |>
    dplyr::pull(mean)
  
  medium.lower <- var.means |>
    dplyr::filter(Var == 'medium eastings') |>
    dplyr::pull(lower)
  
  medium.upper <- var.means |>
    dplyr::filter(Var == 'medium eastings') |>
    dplyr::pull(upper)
  
# plot medium eastings
  
p3 <- center_of_gravity |>
    dplyr::filter(Var == 'medium eastings') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=medium.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Medium (30.3 - 50 cm) Eastings") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # medium northings  
  medium.mean <- var.means |>
    dplyr::filter(Var == 'medium northings') |>
    dplyr::pull(mean)
  
  medium.lower <- var.means |>
    dplyr::filter(Var == 'medium northings') |>
    dplyr::pull(lower)
  
  medium.upper <- var.means |>
    dplyr::filter(Var == 'medium northings') |>
    dplyr::pull(upper)
  
# plot medium northings
  
p4 <- center_of_gravity |>
    dplyr::filter(Var == 'medium northings') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=medium.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Medium (30.3 - 50 cm) Northings") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # large eastings  
  large.mean <- var.means |>
    dplyr::filter(Var == 'large eastings') |>
    dplyr::pull(mean)
  
  large.lower <- var.means |>
    dplyr::filter(Var == 'large eastings') |>
    dplyr::pull(lower)
  
  large.upper <- var.means |>
    dplyr::filter(Var == 'large eastings') |>
    dplyr::pull(upper)
  
# plot large eastings
  
p5 <- center_of_gravity |>
    dplyr::filter(Var == 'large eastings') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=large.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Large (>= 50 cm) Eastings") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # large northings  
  large.mean <- var.means |>
    dplyr::filter(Var == 'large northings') |>
    dplyr::pull(mean)
  
  large.lower <- var.means |>
    dplyr::filter(Var == 'large northings') |>
    dplyr::pull(lower)
  
  large.upper <- var.means |>
    dplyr::filter(Var == 'large northings') |>
    dplyr::pull(upper)
  
# plot large northings
  
p6 <- center_of_gravity |>
    dplyr::filter(Var == 'large northings') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=large.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Large (>= 50 cm) Northings") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # combine plots
  
  p <- cowplot::plot_grid(p1,p2,p3,p4,p5,p6, nrow=3,scale = 0.9) +
    cowplot::draw_label(expression("Center of Gravity (km)"), x=  0, y=0.5, vjust= 1.5, angle=90)
  
  return(p)
  
  
}
