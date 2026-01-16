## -----------------------------------------------------------------------------
## Presence of ARDS Syndrome (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the systematic DAG variable for presence of ARDS syndrome on Day 0
#'
#' `calc_sys_ards_0` calculates the systematic DAG variable for whether
#' ARDS syndrome was present on or before Day 0. This combines both documented
#' presence and clinical judgement assessments.
#'
#' @param ards_present Character vector. The `ards_present` column from the data.
#' Possible values: "Yes", "No", or NA.
#' @param ards_clinical_judgement Character vector. The `ards_clinical_judgement`
#' column from the data. Possible values: "Yes", "No", "Unknown", or NA.
#'
#' @returns A numeric vector with values:
#' - 0 = ARDS syndrome not present or not present on/before Day 0
#' - 1 = ARDS syndrome present on or before Day 0
#' - 99 = Unknown
#' @export
calc_sys_ards_0 <- function(
  ards_present,
  ards_clinical_judgement
  ) {

  ## Check if ARDS is present based on either documented presence or clinical judgement
  dplyr::case_when(
    ards_present == "No" | ards_clinical_judgement == "No" ~ 0,
    ards_present == "Yes" & ards_clinical_judgement == "Yes" ~ 1,
    ards_clinical_judgement == "Unknown" ~ 99
  )
}
