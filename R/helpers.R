
#' Extract the most common value in a vector
#'
#' @param v Vector for which to extract the mode.


calc_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
