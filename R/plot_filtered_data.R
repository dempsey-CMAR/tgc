#' Plot filtered data with removed data greyed out
#'
#' @inheritParams identify_growing_season
#' @param dat_filtered Filtered data, i.e. the result of
#'   \code{apply_dd_filters(dat)}.
#'
#' @return Returns a ggplot object.
#'
#' @importFrom dplyr anti_join mutate
#'
#' @export

plot_filtered_data <- function(dat, dat_filtered,
                               ...,
                               superchill = NULL,
                               heatstress = NULL,
                               trending_up = NULL){

  dat_plot <- dat_filtered %>%
    rbind(
      dat %>%
        anti_join(dat_filtered, ...) %>%
        mutate(DEPTH = 0, SEASON = NA)
    ) %>%
    convert_depth_to_ordered_factor()
  levels(dat_plot$DEPTH)[1] <- "Filtered"

  color.pal <- get_colour_palette(dat_plot)
  color.pal <- c("grey", color.pal)

  axis_breaks <- get_xaxis_breaks(dat_plot)

  ggplot(dat_plot, aes(x = TIMESTAMP, y = VALUE, col = DEPTH)) +
    geom_point(size = 0.25) +

    scale_x_datetime(breaks = axis_breaks$date.breaks.major,
                     minor_breaks = axis_breaks$date.breaks.minor,
                     date_labels =  axis_breaks$date.labels.format) +
    scale_colour_manual(name = "Depth",
                        values = color.pal,
                        drop = FALSE) +
    + geom_hline(yintercept = superchill, col = "deepskyblue", lty = 2) +
    geom_hline(yintercept = trending_up, col = "grey", lty = 2) +
    geom_hline(yintercept = heatstress, col = "red", lty = 2) +
    guides(color = guide_legend(override.aes = list(size = 4)))

}
