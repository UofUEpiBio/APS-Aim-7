## -----------------------------------------------------------------------------
## SCAP Criterion: Respiratory Rate (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for SCAP respiratory rate criterion
#'
#' `calc_str_scap_rr_0` calculates the SCAP (Severe Community-Acquired Pneumonia)
#' respiratory rate criterion using Day 0 value with lookback to Days -1 and -2.
#'
#' QUESTION: Should we be checking any other variables here?
#' - The `daily_resp_rate_8a_*` variables are only for IMV?
#' - Could use `highrr_vsorres`/`lowrr_vsorres` instead
#' @param daily_resp_rate_8a_0 Numeric vector. Respiratory rate on Day 0.
#' @param daily_resp_rate_8a_m1 Numeric vector. Respiratory rate on Day -1.
#' @param daily_resp_rate_8a_m2 Numeric vector. Respiratory rate on Day -2.
#'
#' @returns A numeric vector with values:
#' - 0 = Respiratory rate ≤ 30
#' - 1 = Respiratory rate > 30
#' @export
calc_str_scap_rr_0 <- function(
  daily_resp_rate_8a_0,
  daily_resp_rate_8a_m1,
  daily_resp_rate_8a_m2
  ) {

  ## Get value with lookback (Day 0 -> Day -1 -> Day -2)
  rr <- get_value_with_lookback(daily_resp_rate_8a_0, daily_resp_rate_8a_m1, daily_resp_rate_8a_m2)

  ## Apply SCAP criterion
  dplyr::case_when(
    rr <= 30 ~ 0,
    rr > 30 ~ 1
  )
}
