## -----------------------------------------------------------------------------
## Presence of Pneumonia Syndrome (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Presence of Pneumonia Syndrome'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_pneumonia_0 <- function(
  pna_clinical_judgement
  ) {

  ## Determine pneumonia presence
  dplyr::case_when(
    pna_clinical_judgement == "No" ~ 0,
    pna_clinical_judgement == "Yes" ~ 1,
    is_unknown(pna_clinical_judgement) ~ 99
  )
}
