## -----------------------------------------------------------------------------
## Active Fungal Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate systematic DAG active fungal infection on Day 0
#'
#' Calculates the systematic DAG variable for active fungal infection from
#' medication history and daily treatment data. Identifies patients who were
#' taking or receiving systemic antifungal agents.
#'
#' @inheritParams day0_medications_params
#' @inheritParams daily_treatments_params
#'
#' @returns Integer vector with values:
#' - `0` = No active fungal infection
#' - `1` = Active fungal infection present (on antifungals)
#' - `99` = Unknown
#' @export
calc_sys_active_fungal_0 <- function(
  mhantifungals,
  daily_antifungal_0,
  trx_0
) {
  dplyr::case_when(
    mhantifungals == "No" & (trx_0 == "Not Available" | daily_antifungal_0 == "Not administered") ~ 0,

    mhantifungals == "Yes" | (trx_0 == "Available" & daily_antifungal_0 == "Administered") ~ 1,

    is_unknown(mhantifungals) | daily_antifungal_0 == "UNK" ~ 99
  )
}
