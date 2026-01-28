## -----------------------------------------------------------------------------
## Major Recent Surgery (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Major Recent Surgery'
#
# Values:
# - 0 = No
# - 1 = Yes
calc_str_major_surgery_0 <- function(
  daily_surgery_m2,
  daily_surgery_m1,
  daily_surgery_0
) {

  ## Check if surgery occurred on any of the three days
  had_surgery <- (
    daily_surgery_m2 == "Administered" |
    daily_surgery_m1 == "Administered" |
    daily_surgery_0 == "Administered"
  )

  ## Return binary indicator
  dplyr::case_when(
    had_surgery ~ 1,
    TRUE ~ 0
  )
}
