#' Calculate final body weight using TGC model
#'
#' @details final_weight = (initial_weight^(1/3) + (tgc/1000)*degree_days)^3
#' @param initial_weight Initial weight of the fish
#' @param tgc Thermal growth coefficient
#' @param degree_days Number of degree days experienced
#'
#' @return Returns a single value
#' @export


calculate_final_weight_TGC <-function(initial_weight, tgc, degree_days){

  ( initial_weight^(1/3) + (tgc/100)*degree_days )^3

}

#' Calculate initial body weight using TGC model
#'
#' @param final_weight Final weight of the fish
#' @param tgc Thermal growth coefficient
#' @param degree_days Number of degree days experienced
#' @return Returns a single value
#' @export

calculate_initial_weight_TGC <- function(final_weight, tgc, degree_days){

  ( final_weight^(1/3) - (tgc/100)*degree_days )^3

}


#' Use TGC model to calculate degree days required to grow from initial to final
#' weight
#'
#' @param final_weight Final weight of the fish
#' @param initial_weight Initial weight of the fish
#' @param tgc Thermal growth coefficient
#'
#' @return Returns a single value
#' @export
calculate_degree_days_TGC <- function(initial_weight, final_weight, tgc){

  (final_weight^(1/3) - initial_weight^(1/3) ) * 100/tgc

}


