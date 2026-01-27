## -----------------------------------------------------------------------------
## SCAP Criterion: Hypothermia (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'SCAP Criterion: Hypothermia'
#
# Values:
# - 0 = No (Temperature >= 35.0°C)
# - 1 = Yes (Temperature < 35.0°C)
calc_str_scap_hypothermia_0 <- function(
  lowtemp_vsorres
  ) {

  ## Apply SCAP criterion
  dplyr::case_when(
    lowtemp_vsorres >= 35.0 ~ 0,
    lowtemp_vsorres < 35.0 ~ 1
  )
}
