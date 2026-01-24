## -----------------------------------------------------------------------------
## Age (Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate streamlined DAG age on Day 0
#'
#' Calculates the streamlined DAG variable for age from patient enrollment data.
#' Returns the patient's age in years at time of enrollment.
#'
#' @inheritParams enrollment_demographics_params
#'
#' @returns Numeric vector representing age in years on Day 0.
#' @export
calc_str_age_0 <- function(
  age
) {
  dplyr::case_when(
    !is.na(age) & age >= 0 ~ age
  )
}
