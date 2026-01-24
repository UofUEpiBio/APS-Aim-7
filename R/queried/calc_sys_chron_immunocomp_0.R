## -----------------------------------------------------------------------------
## Chronic Immunocompromise (Systematic DAG)
## -----------------------------------------------------------------------------


#' Calculate systematic DAG chronic immunocompromise on Day 0
#'
#' Calculates the systematic DAG variable for chronic immunocompromise from
#' baseline medical history. Identifies patients with immunocompromising conditions
#' excluding chronic steroid use (transplant, malignancy, HIV, immunosuppressive
#' medications, congenital immunodeficiency).
#'
#' @inheritParams day0_baseline_medical_history_params
#'
#' @returns Integer vector with values:
#' - `0` = No chronic immunocompromise
#' - `1` = Chronic immunocompromise present
#' - `99` = Unknown
#' @export
calc_sys_chron_immunocomp_0 <- function(
  m_immunosuppression,
  m_immunosup_conditions___2,
  m_immunosup_conditions___3,
  m_immunosup_conditions___4,
  m_immunosup_conditions___5,
  m_immunosup_conditions___6,
  m_immunosup_conditions___7,
  m_immunosup_conditions___88
) {
  dplyr::case_when(
    m_immunosuppression %in% c("Yes", "No") &
      is_unchecked(m_immunosup_conditions___2) &
      is_unchecked(m_immunosup_conditions___3) &
      is_unchecked(m_immunosup_conditions___4) &
      is_unchecked(m_immunosup_conditions___5) &
      is_unchecked(m_immunosup_conditions___6) &
      is_unchecked(m_immunosup_conditions___7) &
      is_unchecked(m_immunosup_conditions___88) ~ 0,

    m_immunosuppression == "Yes" & (
      is_checked(m_immunosup_conditions___2) |
      is_checked(m_immunosup_conditions___3) |
      is_checked(m_immunosup_conditions___4) |
      is_checked(m_immunosup_conditions___5) |
      is_checked(m_immunosup_conditions___6) |
      is_checked(m_immunosup_conditions___7) |
      is_checked(m_immunosup_conditions___88)
      ) ~ 1,

    is_unknown(m_immunosuppression) ~ 99
  )
}
