#' Calculate final body weight using TGC model
#'
#' @details Final weight is calculated from the thermal growth coefficient (TGC)
#'   model:
#'
#'   \deqn{final_weight = (initial_weight^(1/3) + (TGC/1000)*degree_days)^3 }
#'
#'   where \eqn{final_weight} and \eqn{initial_weight} are in kilograms.
#'
#'   Final weight will be calculated for all combinations of
#'   \code{initial_weight} and \code{tgc} supplied in the arguments and each row
#'   of \code{dd_table}.
#'
#' @param dd_table Dataframe with degree-day data, as exported from
#'   \code{count_degree_days}. Must include columns \code{n_degree_days}.
#'
#' @param initial_weight A vector of initial weight(s) of the fish in grams.
#'
#' @param tgc A vector of thermal growth coefficient(s).
#'
#' @return Returns \code{dd_table} additional columns \code{INITIAL_WEIGHT},
#'   \code{TGC}, and \code{TGC_FINAL_WEIGHT}.
#'
#' @importFrom dplyr mutate full_join %>% select
#'
#' @export

TGC_calculate_final_weight <-function(dd_table,
                                      initial_weight,
                                      tgc){

  # make a table with all combinations of TGC and initial weight
  n_rows <- nrow(dd_table)
  params <- expand.grid(INDEX = c(1:n_rows),
                        TGC = tgc,
                        INITIAL_WEIGHT = initial_weight)

  # calculate final weight for each row and return
  dd_out <- dd_table %>%
    mutate(INDEX = c(1:n())) %>%
    full_join(params, by = "INDEX") %>%
    mutate(
      TGC_FINAL_WEIGHT = (INITIAL_WEIGHT^(1/3) + (TGC/1000) * n_degree_days)^3,
      TGC_FINAL_WEIGHT = round(TGC_FINAL_WEIGHT, digits = 2),
      CHECK = TGC_FINAL_WEIGHT > INITIAL_WEIGHT
    ) %>%
    select(-INDEX)

  if(sum(dd_out$CHECK) < nrow(dd_out)){

    print(dd_out %>% filter(CHECK == 0))

    stop("Final weight is less than initial weight for above row(s)")

  }

  dd_out %>% select(-CHECK)

}

#' Calculate initial body weight using TGC model
#'
#' @details Initial weight is calculated from the thermal growth coefficient
#'   (TGC) model:
#'
#'   \deqn{initial_weight = (final_weight^(1/3) - (TGC/1000)*degree_days)^3 }
#'
#'   where \eqn{final_weight} and \eqn{initial_weight} are in kilograms.
#'
#'   Initial weight will be calculated for all combinations of
#'   \code{initial_weight} and \code{tgc} supplied in the arguments and each row
#'   of \code{dd_table}.
#'
#' @inheritParams TGC_calculate_final_weight
#'
#' @param final_weight A vector of final weight(s) of the fish in grams.
#'
#' @return Returns \code{dd_table} additional columns \code{FINAL_WEIGHT},
#'   \code{TGC}, and \code{TGC_INITIAL_WEIGHT}.
#'
#' @importFrom dplyr n %>% full_join mutate select
#'
#' @export

TGC_calculate_initial_weight <- function(dd_table,
                                         final_weight,
                                         tgc){

  # make a table with all combinations of TGC and initial weight
  n_rows <- nrow(dd_table)
  params <- expand.grid(INDEX = c(1:n_rows),
                        TGC = tgc,
                        FINAL_WEIGHT = final_weight)

  # calculate initial weight for each row and return
  dd_out <- dd_table %>%
    mutate(INDEX = c(1:n())) %>%
    full_join(params, by = "INDEX") %>%
    mutate(
      TGC_INITIAL_WEIGHT = (FINAL_WEIGHT^(1/3) - (TGC/1000)*n_degree_days)^3,
      TGC_INITIAL_WEIGHT = round(TGC_INITIAL_WEIGHT, digits = 2),
      CHECK = FINAL_WEIGHT > TGC_INITIAL_WEIGHT
    ) %>%
    select(-INDEX)


  if(sum(dd_out$CHECK) < nrow(dd_out)){

    print(dd_out %>% filter(CHECK == 0))

    stop("Final weight is less than initial weight for above row(s)")

  }

  dd_out %>% select(-CHECK)

}


#' Use TGC model to calculate degree days required to grow from initial to final
#' weight
#'
#' @inheritParams TGC_calculate_final_weight
#'
#' @inheritParams TGC_calculate_initial_weight
#'
#' @return Returns a table with columns \code{INITIAL_WEIGHT},
#'   \code{FINAL_WEIGHT}, \code{TGC}, and \code{TGC_DEGREE_DAYS}.
#'
#' @importFrom dplyr mutate
#'
#' @export

TGC_calculate_degree_days <- function(initial_weight,
                                      final_weight,
                                      tgc){


  # make a table with all combinations of TGC and initial weight
  params <- expand.grid(INITIAL_WEIGHT = initial_weight,
                        FINAL_WEIGHT = final_weight,
                        TGC = tgc) %>%
    mutate(CHECK = FINAL_WEIGHT > INITIAL_WEIGHT)

  if(sum(params$CHECK) < nrow(params)){

    print(params %>% filter(CHECK == 0))

    stop("Final weight is less than initial weight for above row(s)")

  }

  # calculate degree days for each row and return
  params %>%
    mutate(
      TGC_DEGREE_DAYS =  (FINAL_WEIGHT^(1/3) - INITIAL_WEIGHT^(1/3) ) * 1000/TGC
    ) %>%
    select(-CHECK)

}


