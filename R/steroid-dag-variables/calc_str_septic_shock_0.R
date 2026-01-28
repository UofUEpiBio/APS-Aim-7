## -----------------------------------------------------------------------------
## Septic Shock (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Septic Shock'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_str_septic_shock_0 <- function(
  sepsis_present,
  sepsis_clinical_judgement,
  daily_vasopressors_0___0
) {
  # ANSWERED: Is is appropriate to use OR is_checked(vasopressors) here? (alterative creates many more NA records)
  # - ANSWER: If final answer (after query) is that 'Unknown' won't be changed, then set those to 0.
  dplyr::case_when(
    sepsis_present == "No" | sepsis_clinical_judgement == "No" | is_checked(daily_vasopressors_0___0) ~ 0,
    sepsis_present == "Yes" & sepsis_clinical_judgement == "Yes" & is_unchecked(daily_vasopressors_0___0) ~ 1,
    is_unknown(sepsis_clinical_judgement) ~ 99
  )
}
