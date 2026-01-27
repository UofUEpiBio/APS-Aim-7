## -----------------------------------------------------------------------------
## SCAP Criterion: Thrombocytopenia (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'SCAP Criterion: Thrombocytopenia'
#
# Values:
# - 0 = No (Platelets >= 100)
# - 1 = Yes (Platelets < 100)
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
