#' Plot filtered data with removed data greyed out
#'
#' @inheritParams identify_growing_seasons
#' @inheritParams identify_heat_stress_intervals
#' @param dat_filtered Filtered data, i.e. the result of
#'   \code{apply_dd_filters(dat)}.
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
                               heat_threshold = 18){

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

  color.pal <- strings::get_colour_palette(dat_plot)
  color.pal <- c("grey", color.pal)

  axis_breaks <- strings::get_xaxis_breaks(dat_plot)

  ggplot(dat_plot, aes(x = TIMESTAMP, y = VALUE, col = DEPTH)) +
    geom_point(size = 0.25) +

    scale_x_datetime(breaks = axis_breaks$date.breaks.major,
                     minor_breaks = axis_breaks$date.breaks.minor,
                     date_labels =  axis_breaks$date.labels.format) +
    scale_y_continuous(name =  expression(paste("Temperature (",degree,"C)"))) +
    scale_colour_manual(name = "Depth",
                        values = color.pal,
                        drop = FALSE) +
    geom_hline(yintercept = superchill_threshold, col = "deepskyblue", lty = 2) +
    geom_hline(yintercept = trend_threshold, col = "grey", lty = 2) +
    geom_hline(yintercept = heat_threshold, col = "red", lty = 2) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme_light()

}
