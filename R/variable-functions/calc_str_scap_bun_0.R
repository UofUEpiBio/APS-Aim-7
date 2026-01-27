## -----------------------------------------------------------------------------
## SCAP Criterion: BUN Value (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'SCAP Criterion: BUN Value'
#
# Values:
# - 0 = No (BUN <= 30 mg/dL)
# - 1 = Yes (BUN > 30 mg/dL)
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
