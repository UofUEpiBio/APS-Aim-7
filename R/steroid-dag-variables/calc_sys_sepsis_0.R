## -----------------------------------------------------------------------------
## Presence of Sepsis Syndrome (Systematic DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Presence of Sepsis Syndrome'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_sepsis_0 <- function(
  sepsis_present,
  sepsis_clinical_judgement
  ) {

  ## Check if sepsis is present based on either documented presence or clinical judgement
  dplyr::case_when(
    sepsis_present == "No" | sepsis_clinical_judgement == "No" ~ 0,
    sepsis_present == "Yes" & sepsis_clinical_judgement == "Yes" ~ 1,
    is_unknown(sepsis_clinical_judgement) ~ 99
  )
}
