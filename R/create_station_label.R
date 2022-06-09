#' Assign a label in the form Station Name Long Season

#' @param dat Data frame with at least one column named \code{STATION}.
#'
#' @param short Station name for the short season example (character string).
#'
#' @param medium Station name for the medium season example (character string).
#'
#' @param long Station name for the long season example (character string).
#'
#' @param sep Label separator. Default is a line break.
#'
#' @return Returns \code{dat} with additional column \code{LABEL}.
#'
#' @importFrom dplyr mutate select
#' @importFrom glue glue
#'
#' @export

create_station_label <- function(dat, short, medium, long, sep = "\n"){
#
#   f_levels <- c(
#     glue("{short} \n (Short Season)"),
#     glue("{medium} \n (Medium Season)"),
#     glue("{long} \n (Long Season)")
#   )

  f_levels <- c(
    glue("{short}{sep}(Short Season)"),
    glue("{medium}{sep}(Medium Season)"),
    glue("{long}{sep}(Long Season)")
  )

  dat %>%
    mutate(
      SEASON = case_when(
        STATION == long ~ "Long Season",
        STATION == medium ~ "Medium Season",
        STATION == short ~ "Short Season"
      ),
      LABEL = glue("{STATION}{sep}({SEASON})"),
      LABEL = ordered(LABEL, levels = f_levels)
    ) %>%
    select(-SEASON)

}
