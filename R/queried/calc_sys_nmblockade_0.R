## -----------------------------------------------------------------------------
## Neuromuscular Blockade (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the systematic DAG variable for neuromuscular blockade on Day 0
#'
#' `calc_sys_nmblockade_0` calculates the systematic DAG variable for
#' neuromuscular blockade from the data. This variable represents daily
#' neuromuscular blocking agent use on admission day.
#'
#' @param daily_paralysis_0 Character vector. The `daily_paralysis_0` column
#' from the data.
#' @param trx_0 Character vector. The `trx_0` column from the data.
#'
#' @returns A vector with values:
#' - 0 = No neuromuscular blockade on day 0 or missing
#' - 1 = Neuromuscular blockade given on day 0
#' @export
calc_sys_nmblockade_0 <- function(daily_paralysis_0, trx_0) {
  dplyr::case_when(
    daily_paralysis_0 == 'Administered' ~ 1,
    trx_0 == 'Available' & !is.na(daily_paralysis_0) ~ 0,
    trx_0 == 'Not Available' & is.na(daily_paralysis_0) ~ 0
  )
}
