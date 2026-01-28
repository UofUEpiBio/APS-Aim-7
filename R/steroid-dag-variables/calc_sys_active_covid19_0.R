## -----------------------------------------------------------------------------
## Active SARS-CoV-2 Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Active SARS-CoV-2 Infection' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
calc_sys_active_covid19_0 <- function(has_pos_covid_0) {
  dplyr::case_when(
    has_pos_covid_0 == 1 ~ 1,
    is.na(has_pos_covid_0) ~ 0
  )
}
