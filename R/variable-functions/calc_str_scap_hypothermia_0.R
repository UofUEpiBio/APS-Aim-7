## -----------------------------------------------------------------------------
## SCAP Criterion: Hypothermia (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for SCAP hypothermia criterion
#'
#' `calc_str_scap_hypothermia_0` calculates the SCAP (Severe Community-Acquired
#' Pneumonia) hypothermia criterion using Day 0 value with lookback to Days -1 and -2.
#'
#' @param lowtemp_vsorres Numeric vector. Lowest temperature on Day 0.
#' @param lowtemp_vsorres_m1 Numeric vector. Lowest temperature on Day -1.
#' @param lowtemp_vsorres_m2 Numeric vector. Lowest temperature on Day -2.
#'
#' @returns A numeric vector with values:
#' - 0 = Temperature ≥ 35.0°C
#' - 1 = Temperature < 35.0°C
#' @export
calc_str_scap_hypothermia_0 <- function(
  lowtemp_vsorres
  ) {

  ## Apply SCAP criterion
  dplyr::case_when(
    lowtemp_vsorres >= 35.0 ~ 0,
    lowtemp_vsorres < 35.0 ~ 1
  )
}
