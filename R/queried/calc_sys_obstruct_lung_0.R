## -----------------------------------------------------------------------------
## Obstructive Lung Disease (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the streamlined DAG variable for obstructive lung disease on Day 0
#'
#' `calc_sys_obstruct_lung_0` calculates the streamlined DAG variable for
#' obstructive lung disease from the data.
#'
#' @param m_pulmonary Character vector. The `m_pulmonary` column from the data.
#' @param m_pulm_conditions___1 Character vector. The `m_pulm_conditions___1` column
#' from the data.
#' @param m_pulm_conditions___3 Character vector. The `m_pulm_conditions___3` column
#' from the data.
#'
#' @returns A vector with values:
#' - 0 = No obstructive lung disease
#' - 1 = Obstructive lung disease present
#' - 99 = Unknown
#' @export
calc_sys_obstruct_lung_0 <- function(
  m_pulmonary,
  m_pulm_conditions___1,
  m_pulm_conditions___3
) {
  dplyr::case_when(
    m_pulmonary == "Yes" &
      (is_checked(m_pulm_conditions___1) | is_checked(m_pulm_conditions___3)) ~ 1,
    m_pulmonary %in% c("Yes", "No") &
      (is_unchecked(m_pulm_conditions___1) & is_unchecked(m_pulm_conditions___3)) ~ 0,
    m_pulmonary == "Unknown" ~ 99
  )
}
