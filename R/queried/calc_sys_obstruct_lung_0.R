## -----------------------------------------------------------------------------
## Obstructive Lung Disease (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate systematic DAG obstructive lung disease on Day 0
#'
#' Calculates the systematic DAG variable for obstructive lung disease from
#' baseline medical history. Identifies patients with COPD or asthma history.
#'
#' @inheritParams day0_baseline_medical_history_params
#'
#' @returns Integer vector with values:
#' - `0` = No obstructive lung disease
#' - `1` = Obstructive lung disease present (COPD or asthma)
#' - `99` = Unknown
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
    is_unknown(m_pulmonary) ~ 99
  )
}
