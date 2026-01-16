## -----------------------------------------------------------------------------
## SCAP Criterion: Thrombocytopenia (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for SCAP thrombocytopenia criterion
#'
#' `calc_str_scap_thrombocytopenia_0` calculates the SCAP (Severe Community-Acquired
#' Pneumonia) thrombocytopenia criterion using Day 0 value with lookback to Days -1 and -2.
#'
#' @param daily_platelet_8a_0 Numeric vector. Platelet count on Day 0.
#' @param daily_platelet_8a_m1 Numeric vector. Platelet count on Day -1.
#' @param daily_platelet_8a_m2 Numeric vector. Platelet count on Day -2.
#'
#' @returns A numeric vector with values:
#' - 0 = Platelets >= 100
#' - 1 = Platelets < 100
#' @export
calc_str_scap_thrombocytopenia_0 <- function(
  daily_platelet_8a_0,
  daily_platelet_8a_m1,
  daily_platelet_8a_m2
  ) {

  ## Get value with lookback (Day 0 -> Day -1 -> Day -2)
  platelets <- get_value_with_lookback(daily_platelet_8a_0, daily_platelet_8a_m1, daily_platelet_8a_m2)

  ## Apply SCAP criterion
  dplyr::case_when(
    platelets >= 100 ~ 0,
    platelets < 100 ~ 1
  )
}
