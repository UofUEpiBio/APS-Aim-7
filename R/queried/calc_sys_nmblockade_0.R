## -----------------------------------------------------------------------------
## Neuromuscular Blockade (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate systematic DAG neuromuscular blockade on Day 0
#'
#' Calculates the systematic DAG variable for neuromuscular blockade from daily
#' treatment data. Represents whether chemical paralysis (other than during
#' intubation) was administered on admission day (Day 0).
#'
#' @inheritParams daily_treatments_params
#'
#' @returns Integer vector with values:
#' - `0` = No neuromuscular blockade on Day 0 or not applicable
#' - `1` = Neuromuscular blockade administered on Day 0
#' @export
calc_sys_nmblockade_0 <- function(daily_paralysis_0, trx_0) {
  dplyr::case_when(
    daily_paralysis_0 == 'Administered' ~ 1,
    trx_0 == 'Available' & !is.na(daily_paralysis_0) ~ 0,
    trx_0 == 'Not Available' & is.na(daily_paralysis_0) ~ 0
  )
}
