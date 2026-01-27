## -----------------------------------------------------------------------------
## Presence of ARDS Syndrome (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Presence of ARDS Syndrome'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_ards_0 <- function(
  ards_present,
  ards_clinical_judgement
  ) {

  ## Check if ARDS is present based on either documented presence or clinical judgement
  dplyr::case_when(
    ards_present == "No" | ards_clinical_judgement == "No" ~ 0,
    ards_present == "Yes" & ards_clinical_judgement == "Yes" ~ 1,
    is_unknown(ards_clinical_judgement) ~ 99
  )
}
