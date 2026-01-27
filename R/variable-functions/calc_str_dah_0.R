## -----------------------------------------------------------------------------
## Diffuse Alveolar Hemorrhage (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Diffuse Alveolar Hemorrhage'
#
# Values:
# - 0 = No
# - 1 = Yes
calc_str_dah_0 <- function(
  organ_dysfnx_cause_code
  ) {

  dplyr::case_when(
    organ_dysfnx_cause_code %in% c(1, 3, 99) ~ 0,
    organ_dysfnx_cause_code == 2 ~ 1
  )
}
