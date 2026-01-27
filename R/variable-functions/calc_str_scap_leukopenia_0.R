## -----------------------------------------------------------------------------
## SCAP Criterion: Leukopenia (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'SCAP Criterion: Leukopenia'
#
# Values:
# - 0 = No (WBC >= 4)
# - 1 = Yes (WBC < 4)
calc_str_scap_leukopenia_0 <- function(
  daily_wbc_8a_0,
  daily_wbc_8a_m1,
  daily_wbc_8a_m2
  ) {

  ## Get value with lookback (Day 0 -> Day -1 -> Day -2)
  wbc <- get_value_with_lookback(daily_wbc_8a_0, daily_wbc_8a_m1, daily_wbc_8a_m2)

  ## Apply SCAP criterion
  dplyr::case_when(
    wbc >= 4 ~ 0,
    wbc < 4 ~ 1
  )
}
