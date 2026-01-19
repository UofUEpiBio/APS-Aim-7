## -----------------------------------------------------------------------------
## Septic Shock (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for septic shock on Day 0
#'
#' `calc_str_septic_shock_0` calculates the streamlined DAG variable for
#' septic shock, defined as sepsis with any vasopressor use at 8am on Day 0.
#'
#' @param sepsis_present Character vector. The `sepsis_present` column from the data.
#' @param sepsis_clinical_judgement Character vector. The `sepsis_clinical_judgement` column.
#' @param daily_vasopressors_0___0 Character vector. Checkbox for "no vasopressors". Checked = no vasopressors, Unchecked = on vasopressors.
#'
#' @returns A numeric vector with values:
#' - 0 = Septic shock not present
#' - 1 = Septic shock present
#' - 99 = Unknown
#' @export
calc_str_septic_shock_0 <- function(
  sepsis_present,
  sepsis_clinical_judgement,
  daily_vasopressors_0___0
) {
  # QUESTION: Is is appropriate to use OR is_checked(vasopressors) here? (alterative creates many more NA records)
  dplyr::case_when(
    sepsis_present == "No" | sepsis_clinical_judgement == "No" | is_checked(daily_vasopressors_0___0) ~ 0,
    sepsis_present == "Yes" & sepsis_clinical_judgement == "Yes" & is_unchecked(daily_vasopressors_0___0) ~ 1,
    sepsis_clinical_judgement == "Unknown" ~ 99
  )
}
