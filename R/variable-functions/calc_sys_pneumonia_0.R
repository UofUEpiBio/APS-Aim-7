## -----------------------------------------------------------------------------
## Presence of Pneumonia Syndrome (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the systematic DAG variable for presence of pneumonia syndrome on Day 0
#'
#' `calc_sys_pneumonia_0` calculates the systematic DAG variable for whether
#' pneumonia syndrome was present on or before Day 0. This combines both
#' clinical judgement and date assessments.
#'
#' @param pna_clinical_judgement Character vector. The `pna_clinical_judgement`
#' column from the data. Possible values: "Yes", "No", "Unknown", or NA.
#'
#' @returns A numeric vector with values:
#' - 0 = Pneumonia syndrome not present on/before Day 0
#' - 1 = Pneumonia syndrome present on/before Day 0
#' - 99 = Unknown
#' @export
calc_sys_pneumonia_0 <- function(
  pna_clinical_judgement
  ) {

  ## Determine pneumonia presence
  dplyr::case_when(
    pna_clinical_judgement == "No" ~ 0,
    pna_clinical_judgement == "Yes" ~ 1,
    pna_clinical_judgement == "Unknown" ~ 99
  )
}
