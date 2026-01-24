## -----------------------------------------------------------------------------
## Active SARS-CoV-2 Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate systematic DAG active COVID-19 infection on Day 0
#'
#' Calculates the systematic DAG variable for active SARS-CoV-2 (COVID-19)
#' infection from microbiology testing data. Identifies patients with a positive
#' COVID-19 test on or before enrollment.
#'
#' @param has_pos_covid_0 Numeric vector. Indicator (1) if patient had a positive
#'   COVID-19 (SARS-CoV-2) test on or before enrollment, NA otherwise. This is a
#'   derived variable calculated from microbiology testing data across multiple
#'   events.
#'
#' @returns Integer vector with values:
#' - `0` = No active COVID-19 (not positive on or before Day 0, or not tested)
#' - `1` = Active COVID-19 (tested positive on or before Day 0)
#' @export
calc_sys_active_covid19_0 <- function(has_pos_covid_0) {
  dplyr::case_when(
    has_pos_covid_0 == 1 ~ 1,
    is.na(has_pos_covid_0) ~ 0
  )
}
