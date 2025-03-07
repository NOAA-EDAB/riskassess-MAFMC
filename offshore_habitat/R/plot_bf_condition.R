#' plot Bluefish Condition
#'
#' Plots the relative condition of bluefish in the North Atlantic
#' by size class
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

plot_bf_condition <- function(shadedRegion = NULL,
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
  load(here::here('offshore_habitat/data/bf_condition.rda'))
  
  # calculate mean and sd for reference lines on plots
  # not using sd for now but calculating in case we want to add later
  var.means <- relative_condition |>
    dplyr::group_by(Var) |>
    dplyr::mutate(mean = mean(Value, na.rm = TRUE)) |>
    dplyr::mutate(sd = sd(Value, na.rm = TRUE)) |>
    dplyr::mutate(upper = mean + sd) |>
    dplyr::mutate(lower = mean - sd) |>
    dplyr::ungroup() |> 
    dplyr::select(Var, mean, upper, lower) |>
    dplyr::distinct()
  
  
  
  # small spring 
  small.mean <- var.means |>
    dplyr::filter(Var == 'small (<=30.3cm) spring condition') |>
    dplyr::pull(mean)
  
  small.lower <- var.means |>
    dplyr::filter(Var == 'small (<=30.3cm) spring condition') |>
    dplyr::pull(lower)
  
  small.upper <- var.means |>
    dplyr::filter(Var == 'small (<=30.3cm) spring condition') |>
    dplyr::pull(upper)
  
  # plot small spring
  
  p1 <-  relative_condition |>
    dplyr::filter(Var == 'small (<=30.3cm) spring condition') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=small.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Small Bluefish (<=30.3cm) Spring Condition") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # small fall
  small.mean <- var.means |>
    dplyr::filter(Var == 'small (<=30.3cm) fall condition') |>
    dplyr::pull(mean)
  
  small.lower <- var.means |>
    dplyr::filter(Var == 'small (<=30.3cm) fall condition') |>
    dplyr::pull(lower)
  
  small.upper <- var.means |>
    dplyr::filter(Var == 'small (<=30.3cm) fall condition') |>
    dplyr::pull(upper)
  
  # plot small fall
  
  p2 <-  relative_condition |>
    dplyr::filter(Var == 'small (<=30.3cm) fall condition') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=small.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Small Bluefish (<=30.3cm) Fall Condition") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # medium spring
  medium.mean <- var.means |>
    dplyr::filter(Var == 'medium (30.3-50.0cm) spring condition') |>
    dplyr::pull(mean)
  
  medium.lower <- var.means |>
    dplyr::filter(Var == 'medium (30.3-50.0cm) spring condition') |>
    dplyr::pull(lower)
  
  medium.upper <- var.means |>
    dplyr::filter(Var == 'medium (30.3-50.0cm) spring condition') |>
    dplyr::pull(upper)
  
  # plot medium spring
  
  p3 <- relative_condition |>
    dplyr::filter(Var == 'medium (30.3-50.0cm) spring condition') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=medium.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Medium Bluefish (30.3-50cm) Spring Condition") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # medium fall
  medium.mean <- var.means |>
    dplyr::filter(Var == 'medium (30.3-50.0cm) fall condition') |>
    dplyr::pull(mean)
  
  medium.lower <- var.means |>
    dplyr::filter(Var == 'medium (30.3-50.0cm) fall condition') |>
    dplyr::pull(lower)
  
  medium.upper <- var.means |>
    dplyr::filter(Var == 'medium (30.3-50.0cm) fall condition') |>
    dplyr::pull(upper)
  
  # plot medium fall
  
  p4 <- relative_condition |>
    dplyr::filter(Var == 'medium (30.3-50.0cm) fall condition') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=medium.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Medium Bluefish (30.3-50cm) Fall Condition") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
    
  # large spring
  large.mean <- var.means |>
    dplyr::filter(Var == 'large (>=50.0cm) spring condition') |>
    dplyr::pull(mean)
  
  large.lower <- var.means |>
    dplyr::filter(Var == 'large (>=50.0cm) spring condition') |>
    dplyr::pull(lower)
  
  large.upper <- var.means |>
    dplyr::filter(Var == 'large (>=50.0cm) spring condition') |>
    dplyr::pull(upper)
  
  # plot large spring
  
  p5 <- relative_condition |>
    dplyr::filter(Var == 'large (>=50.0cm) spring condition') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=large.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Large Bluefish (>=50cm) Spring Condition") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()
  
  # large fall
  large.mean <- var.means |>
    dplyr::filter(Var == 'large (>=50.0cm) fall condition') |>
    dplyr::pull(mean)
  
  large.lower <- var.means |>
    dplyr::filter(Var == 'large (>=50.0cm) fall condition') |>
    dplyr::pull(lower)
  
  large.upper <- var.means |>
    dplyr::filter(Var == 'large (>=50.0cm) fall condition') |>
    dplyr::pull(upper)
  
  # plot large fall
  
  p6 <- relative_condition |>
    dplyr::filter(Var == 'large (>=50.0cm) fall condition') |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::geom_hline(yintercept=large.mean,linetype=setup$hline.lty)+
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Large Bluefish (>=50cm) Fall Condition") +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()

  # combine plots
  
  p <- cowplot::plot_grid(p1,p2,p3,p4,p5,p6, nrow=3,scale = 0.9) +
    cowplot::draw_label(expression("Relative condition"), x=  0, y=0.5, vjust= 1.5, angle=90)
  
  return(p)
  
  }