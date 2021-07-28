#' Plot temperature coloured by depth
#'
#' @inheritParams identify_heat_stress_intervals
#' @inheritParams filter_growing_seasons
#'
#' @param facet_var Variable(s) defining faceting groups. Variables must be
#'   column(s) in \code{dat}). For a single facet variable: \code{facet_var =
#'   "SEASON"}. For more than one facet variables: \code{facet_var = "SEASON +
#'   DEPTH"}. Default is \code{facet_var = NULL}.
#'
#' @param ncol Number of columns for faceted figure. Default is \code{ncol = 1}.
#'
#' @param nrow Number of rows for faceted figure. Default is \code{nrow = NULL}.
#'
#' @param alpha Transparency for the heat stress and superchill shaded boxes.
#'
#' @return ggplot object
#'
#' @import ggplot2
#' @importFrom stringr str_detect
#' @importFrom stats as.formula
#' @importFrom dplyr select distinct
#' @importFrom strings convert_depth_to_ordered_factor get_colour_palette
#'   get_xaxis_breaks
#'
#' @export
#'

plot_temperature_at_depth <- function(dat,
                                      trend_threshold = 4,
                                      superchill_threshold = -0.7,
                                      heat_threshold = 18,
                                      facet_var = NULL,
                                      ncol = 1,
                                      nrow = NULL,
                                      alpha = 1){

  # observations can be duplicated for consecutive seasons.
  # if not faceted by season, remove duplicates
  if("SEASON" %in% colnames(dat)){

    if(is.null(facet_var)){
      dat <- dat %>%
        select(-SEASON) %>%
        distinct()
    }

    if(is.character(facet_var)) {

      if(isFALSE(stringr::str_detect(facet_var, "SEASON"))){
        dat <- dat %>%
          select(-SEASON) %>%
          distinct()
      }
    }
  }

  dat <- dat %>%
    strings::convert_depth_to_ordered_factor()

  color.pal <- strings::get_colour_palette(dat)

  p <- ggplot(dat, aes(x = TIMESTAMP, y = VALUE, col = DEPTH)) +
    annotate("rect",
             xmin = as_datetime(-Inf), xmax = as_datetime(Inf),
             ymin = heat_threshold,  ymax = Inf,
             fill = "#FB9A99", alpha = alpha) +
    annotate("rect",
             xmin = as_datetime(-Inf), xmax = as_datetime (Inf),
             ymin = -Inf, ymax = superchill_threshold,
             fill = "#A6CEE3",  alpha = alpha) +
    geom_point(size = 0.25) +
    scale_y_continuous(name =  expression(paste("Temperature (",degree,"C)"))) +
    scale_colour_manual(name = "Depth (m)",
                        values = color.pal,
                        drop = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    geom_hline(yintercept = trend_threshold, col = "grey", lty = 2) +
    theme_light() +
    theme(strip.background = element_rect(fill = NA),
          strip.text = element_text(color = "black", hjust = 0))

  if(is.null(facet_var)){

    axis.breaks <- strings::get_xaxis_breaks(dat)

    p <- p +
      scale_x_datetime(
        name = "Date",
        date_breaks = axis.breaks$date.breaks.major,
        date_minor_breaks = axis.breaks$date.breaks.minor,
        date_labels =  axis.breaks$date.labels.format
      )
  }


  if(is.character(facet_var))  {

    facet_var <- as.formula(paste("~", facet_var))

    p <- p +
      facet_wrap(facet_var, ncol = ncol, nrow = nrow,
                 labeller = label_wrap_gen(multi_line=FALSE)) +
      scale_x_datetime(name = "Date")
  }

  p
}


