## -----------------------------------------------------------------------------
## SCAP Criterion: Acidosis (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for SCAP acidosis criterion
#'
#' `calc_str_scap_acidosis_0` calculates the SCAP (Severe Community-Acquired Pneumonia)
#' acidosis criterion using Day 0 value with lookback to Days -1 and -2.
#'
#' @param daily_ph_lowest_0 Numeric vector. Lowest pH on Day 0.
#' @param daily_ph_lowest_m1 Numeric vector. Lowest pH on Day -1.
#' @param daily_ph_lowest_m2 Numeric vector. Lowest pH on Day -2.
#'
#' @returns A numeric vector with values:
#' - 0 = pH ≥ 7.30
#' - 1 = pH < 7.30
#' @export
calc_str_scap_acidosis_0 <- function(
  daily_ph_lowest_0,
  daily_ph_lowest_m1,
  daily_ph_lowest_m2
  ) {

  ## Get value with lookback (Day 0 -> Day -1 -> Day -2)
  ph <- get_value_with_lookback(daily_ph_lowest_0, daily_ph_lowest_m1, daily_ph_lowest_m2)

  ## Apply SCAP criterion
  dplyr::case_when(
    ph >= 7.30 ~ 0,
    ph < 7.30 ~ 1
  )
}
