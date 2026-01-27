## -----------------------------------------------------------------------------
## SCAP Criterion: Acidosis (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'SCAP Criterion: Acidosis'
#
# Values:
# - 0 = No (pH >= 7.30)
# - 1 = Yes (pH < 7.30)
calc_str_scap_acidosis_0 <- function(
  daily_ph_lowest_0,
  daily_ph_lowest_m1,
  daily_ph_lowest_m2
  ) {

  ## Get value with lookback (Day 0 -> Day -1 -> Day -2)
  ph <- get_value_with_lookback(daily_ph_lowest_0, daily_ph_lowest_m1, daily_ph_lowest_m2)

  ## Apply SCAP criterion
  dplyr::case_when(
    ph < 7.30 ~ 1,
    # ph >= 7.30 or missing
    TRUE ~ 0
  )
}
