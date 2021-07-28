#' Plot degree days at depth for each season start date
#'
#' @inheritParams plot_temperature_at_depth
#'
#' @param dd_table Dataframe with degree-day data, as exported from
#'   \code{count_degree_days}. Must include columns \code{DEPTH},
#'   \code{START_SEASON}, \code{n_degree_days}.
#'
#' @return ggplot object
#'
#' @import ggplot2
#' @importFrom stats as.formula
#' @importFrom strings convert_depth_to_ordered_factor get_colour_palette
#'   get_xaxis_breaks
#'
#' @export
#'

plot_degree_days <- function(dd_table,
                             facet_var = NULL,
                             ncol = 1,
                             nrow = NULL){


  dd_table <- dd_table %>%
    strings::convert_depth_to_ordered_factor()

  col_pal <- strings::get_colour_palette(dd_table)

  p <- ggplot(dd_table, aes(x = START_SEASON, y = n_degree_days, col = DEPTH)) +
    geom_point(size = 2) +
    geom_line(show.legend = FALSE) +
    scale_colour_manual(name = "Depth (m)", values = col_pal) +
    guides(col = guide_legend(override.aes = list(size = 4))) +
    theme_light() +
    theme(strip.background = element_rect(fill = NA),
          strip.text = element_text(color = "black", hjust = 0))

  if(is.null(facet_var)){

    axis.breaks <- strings::get_xaxis_breaks(dd_table)

    p <- p +
      scale_x_datetime(
        breaks = axis.breaks$date.breaks.major,
        minor_breaks = axis.breaks$date.breaks.minor,
        date_labels =  axis.breaks$date.labels.format
      )
  }


  if(is.character(facet_var))  {

    facet_var <- as.formula(paste("~", facet_var))

    p <- p +
      facet_wrap(facet_var, ncol = ncol, nrow = nrow)
  }

  p
}


