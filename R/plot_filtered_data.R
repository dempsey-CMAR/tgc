#' Plot filtered data with removed data greyed out
#'
#' @inheritParams identify_growing_seasons
#' @inheritParams identify_heat_stress_intervals
#' @inheritParams plot_temperature_at_depth
#'
#' @param dat_filtered Filtered data, i.e. the result of
#'   \code{apply_dd_filters(dat)}.
#'
#' @param plotly_friendly Logical argument. If TRUE, y-axis label is set to a
#'   plotly-friendly title ("Temperature (deg C)"). If FALSE,
#'   \code{expression()} is used to insert the degree symbol.
#'
#' @return Returns a ggplot object.
#'
#' @importFrom dplyr anti_join mutate
#' @importFrom ggplot2 ggplot geom_point scale_x_datetime  scale_colour_manual
#'   geom_hline guides aes guide_legend
#' @importFrom strings convert_depth_to_ordered_factor get_colour_palette
#'   get_xaxis_breaks
#' @export

plot_filtered_data <- function(dat, dat_filtered,
                               trend_threshold = 4,
                               superchill_threshold = -0.7,
                               heat_threshold = 18,

                               colour_palette = NULL,

                               date_breaks_major = NULL,
                               date_breaks_minor = NULL,
                               date_labels_format = NULL,

                               plotly_friendly = FALSE,
                               alpha = 1){


# identify filtered observations -------------------------------------------------------------

  dat <- dat %>% mutate(DEPTH = as.character(DEPTH))
  dat_filtered <- dat_filtered %>% mutate(DEPTH = as.character(DEPTH))

  dat_plot <- dat_filtered %>%
    # remove observations included in more than one SEASON
    select(-SEASON) %>%
    distinct() %>%
    # use anti-join to identify observations that were removed
    rbind(
      dat %>%
        anti_join(dat_filtered) %>%
        mutate(DEPTH = 0)
    ) %>%
    strings::convert_depth_to_ordered_factor()
  levels(dat_plot$DEPTH)[1] <- "Filtered"


# pick colour palette and xaxis breaks ------------------------------------

  if(is.null(colour_palette)) colour_pal <- strings::get_colour_palette(dat_plot)

  colour_pal <- c("grey", colour_pal)


  axis_breaks <- strings::get_xaxis_breaks(dat_plot)

  if(!is.null(date_breaks_major)) axis_breaks$date.breaks.major <- date_breaks_major
  if(!is.null(date_breaks_minor)) axis_breaks$date.breaks.minor <- date_breaks_minor
  if(!is.null(date_labels_format)) axis_breaks$date.labels.format <- date_labels_format


# y-axis label ------------------------------------------------------------

if(isFALSE(plotly_friendly)){

  y_axis <- scale_y_continuous(name =  expression(paste("Temperature (",degree,"C)")))

} else {

  y_axis <- scale_y_continuous(name =  "Temperature (deg C)")
}

# plot --------------------------------------------------------------------

  ggplot(dat_plot, aes(x = TIMESTAMP, y = VALUE, col = DEPTH)) +

    annotate("rect",
             xmin = as_datetime(-Inf), xmax = as_datetime(Inf),
             ymin = heat_threshold,  ymax = Inf,
             fill = "#FB9A99", alpha = alpha) +
    annotate("rect",
             xmin = as_datetime(-Inf), xmax = as_datetime (Inf),
             ymin = -Inf, ymax = superchill_threshold,
             fill = "#A6CEE3",  alpha = alpha) +

    geom_point(size = 0.25) +

    scale_x_datetime(name = "Date",
                     breaks = axis_breaks$date.breaks.major,
                     minor_breaks = axis_breaks$date.breaks.minor,
                     date_labels =  axis_breaks$date.labels.format) +
    y_axis +
    scale_colour_manual(name = "Depth (m)",
                        values = colour_pal,
                        drop = FALSE) +
    geom_hline(yintercept = trend_threshold, col = "grey", lty = 2) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme_light()

}
