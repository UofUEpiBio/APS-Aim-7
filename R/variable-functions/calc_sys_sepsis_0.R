## -----------------------------------------------------------------------------
## Presence of Sepsis Syndrome (Systematic DAG)
## -----------------------------------------------------------------------------

#' Calculate the systematic DAG variable for presence of sepsis syndrome on Day 0
#'
#' `calc_sys_sepsis_0` calculates the systematic DAG variable for whether
#' sepsis syndrome was present on or before Day 0. This combines both documented
#' presence and clinical judgement assessments.
#'
#' @param sepsis_present Character vector. The `sepsis_present` column from the data.
#' Possible values: "Yes", "No", or NA.
#' @param sepsis_clinical_judgement Character vector. The `sepsis_clinical_judgement`
#' column from the data. Possible values: "Yes", "No", "Unknown", or NA.
#'
#' @returns A numeric vector with values:
#' - 0 = Sepsis syndrome not present or not present on/before Day 0
#' - 1 = Sepsis syndrome present on or before Day 0
#' - 99 = Unknown
#' @export
calc_sys_sepsis_0 <- function(
  sepsis_present,
  sepsis_clinical_judgement
  ) {

  ## Check if sepsis is present based on either documented presence or clinical judgement
  dplyr::case_when(
    sepsis_present == "No" | sepsis_clinical_judgement == "No" ~ 0,
    # QUESTION: Should this be OR instead of AND?
    # - OR yields no NA (the only NA has NA for one and "Yes" for the other)
    # - If using OR, then which of this or the above condition should happen first?
    sepsis_present == "Yes" & sepsis_clinical_judgement == "Yes" ~ 1,
    sepsis_clinical_judgement == "Unknown" ~ 99
  )
}
