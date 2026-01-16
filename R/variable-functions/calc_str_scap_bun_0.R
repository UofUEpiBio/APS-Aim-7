## -----------------------------------------------------------------------------
## SCAP Criterion: BUN Value (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for SCAP BUN criterion
#'
#' `calc_str_scap_bun_0` calculates the SCAP (Severe Community-Acquired Pneumonia)
#' BUN criterion using Day 0 value with lookback to Days -1 and -2.
#'
#' @param daily_bun_8a_0 Numeric vector. BUN on Day 0.
#' @param daily_bun_8a_m1 Numeric vector. BUN on Day -1.
#' @param daily_bun_8a_m2 Numeric vector. BUN on Day -2.
#'
#' @returns A numeric vector with values:
#' - 0 = BUN <= 30 mg/dL
#' - 1 = BUN > 30 mg/dL
#' @export
calc_str_scap_bun_0 <- function(
  daily_bun_8a_0,
  daily_bun_8a_m1,
  daily_bun_8a_m2
  ) {

  ## Get value with lookback (Day 0 -> Day -1 -> Day -2)
  bun <- get_value_with_lookback(daily_bun_8a_0, daily_bun_8a_m1, daily_bun_8a_m2)

  ## Apply SCAP criterion
  dplyr::case_when(
    bun <= 30 ~ 0,
    bun > 30 ~ 1
  )
}
