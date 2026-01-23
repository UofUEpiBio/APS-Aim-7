## -----------------------------------------------------------------------------
## Active Fungal Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the systematic DAG variable for active fungal infection on Day 0
#'
#' `calc_sys_active_fungal_0` calculates the systematic DAG variable for
#' active fungal infection from the data.
#'
#' @param mhantifungals Character vector. The `mhantifungals` column from the data.
#' @param daily_antifungal_0 Character vector. The `daily_antifungal_0` column from the data.
#' @param trx_0 Character vector. The `trx_0` column from the data.
#'
#' @returns A vector with values:
#' - 0 = No active fungal infection
#' - 1 = Active fungal infection present
#' - 99 = Unknown
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
