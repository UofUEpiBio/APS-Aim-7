## -----------------------------------------------------------------------------
## Active SARS-CoV-2 Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable for active COVID-19 infection on Day 0
#'
#' `calc_sys_active_covid19_0` calculates the systematic DAG variable for
#' active COVID-19 infection from the data.
#'
#' @param has_pos_covid_0 Numeric vector. Indicator (1) if patient had a positive
#' COVID-19 test on or before enrollment, NA otherwise.
#'
#' @returns A vector with values:
#' - 0 = No active COVID-19 (not positive on or before Day 0, or not tested)
#' - 1 = Active COVID-19 (tested positive on or before Day 0)
#' @export
calc_sys_active_covid19_0 <- function(has_pos_covid_0) {
  dplyr::case_when(
    has_pos_covid_0 == 1 ~ 1,
    is.na(has_pos_covid_0) ~ 0
  )
}
