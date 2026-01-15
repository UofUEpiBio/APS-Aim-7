## -----------------------------------------------------------------------------
## SCAP Criterion: Leukopenia (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for SCAP leukopenia criterion
#'
#' `calc_str_scap_leukopenia_0` calculates the SCAP (Severe Community-Acquired Pneumonia)
#' leukopenia criterion using Day 0 value with lookback to Days -1 and -2.
#'
#' @param daily_wbc_8a_0 Numeric vector. WBC on Day 0.
#' @param daily_wbc_8a_m1 Numeric vector. WBC on Day -1.
#' @param daily_wbc_8a_m2 Numeric vector. WBC on Day -2.
#'
#' QUESTION: How to handle 'Not Collected'?
#' @returns A numeric vector with values:
#' - 0 = WBC < 4
#' - 1 = WBC >= 4
#' @export
calc_str_scap_leukopenia_0 <- function(
  daily_wbc_8a_0,
  daily_wbc_8a_m1,
  daily_wbc_8a_m2
  ) {

  ## Get value with lookback (Day 0 -> Day -1 -> Day -2)
  wbc <- get_value_with_lookback(daily_wbc_8a_0, daily_wbc_8a_m1, daily_wbc_8a_m2)

  ## Apply SCAP criterion
  # QUESTION: Should these be reversed?
  dplyr::case_when(
    wbc < 4 ~ 0,
    wbc >= 4 ~ 1
  )
}
