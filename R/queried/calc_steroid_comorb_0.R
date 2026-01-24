## -----------------------------------------------------------------------------
## Steroid Responsive Comorbidity (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate systematic DAG steroid responsive comorbidity on Day 0
#'
#' Calculates the systematic DAG variable for steroid responsive comorbidity from
#' baseline medical history. Identifies patients with rheumatologic conditions or
#' interstitial lung disease that typically respond to steroid therapy.
#'
#' @inheritParams day0_baseline_medical_history_params
#'
#' @returns Integer vector with values:
#' - `0` = No steroid responsive comorbidity
#' - `1` = Steroid responsive comorbidity present
#' - `99` = Unknown
#' @export
calc_sys_steroid_comorb_0 <- function(
  mhrheumd,
  m_rheum_conditions___1,
  m_rheum_conditions___2,
  m_rheum_conditions___3,
  m_rheum_conditions___4,
  m_rheum_conditions___5,
  m_rheum_conditions___6,
  m_rheum_conditions___7,
  m_rheum_conditions___8,
  m_rheum_conditions___88,
  m_pulmonary,
  m_pulm_conditions___6
  ) {
    dplyr::case_when(
      mhrheumd == "No" &
      is_unchecked(m_rheum_conditions___1) &
      is_unchecked(m_rheum_conditions___2) &
      is_unchecked(m_rheum_conditions___3) &
      is_unchecked(m_rheum_conditions___4) &
      is_unchecked(m_rheum_conditions___5) &
      is_unchecked(m_rheum_conditions___6) &
      is_unchecked(m_rheum_conditions___7) &
      is_unchecked(m_rheum_conditions___8) &
      is_unchecked(m_rheum_conditions___88) &
      m_pulmonary %in% c("Yes", "No") &
      is_unchecked(m_pulm_conditions___6) ~ 0,

      (mhrheumd == "Yes" & (
        is_checked(m_rheum_conditions___1) |
        is_checked(m_rheum_conditions___2) |
        is_checked(m_rheum_conditions___3) |
        is_checked(m_rheum_conditions___4) |
        is_checked(m_rheum_conditions___5) |
        is_checked(m_rheum_conditions___6) |
        is_checked(m_rheum_conditions___7) |
        is_checked(m_rheum_conditions___8) |
        is_checked(m_rheum_conditions___88)
        )
      ) |
      (m_pulmonary == "Yes" &
      is_checked(m_pulm_conditions___6)) ~ 1,

      is_unknown(mhrheumd) | is_unknown(m_pulmonary) ~ 99
  )
}


#' Calculate streamlined DAG steroid responsive comorbidity on Day 0
#'
#' Calculates the streamlined DAG variable for steroid responsive comorbidity from
#' baseline medical history and medications. Identifies patients with rheumatologic
#' conditions or metastatic cancer who are on steroids.
#'
#' @inheritParams day0_baseline_medical_history_params
#' @inheritParams day0_medications_params
#'
#' @inherit calc_sys_steroid_comorb_0 return
#' @export
calc_str_steroid_comorb_0 <- function(
  mhrheumd,
  m_rheum_conditions___1,
  m_rheum_conditions___2,
  m_rheum_conditions___3,
  m_rheum_conditions___4,
  m_rheum_conditions___5,
  m_rheum_conditions___6,
  m_rheum_conditions___7,
  m_rheum_conditions___88,
  m_immunosuppression,
  m_immunosup_conditions___4,
  mhccster
) {
  dplyr::case_when(
    (mhrheumd == "No" &
      is_unchecked(m_rheum_conditions___1) &
      is_unchecked(m_rheum_conditions___2) &
      is_unchecked(m_rheum_conditions___3) &
      is_unchecked(m_rheum_conditions___4) &
      is_unchecked(m_rheum_conditions___5) &
      is_unchecked(m_rheum_conditions___6) &
      is_unchecked(m_rheum_conditions___7) &
      is_unchecked(m_rheum_conditions___88)) |
    (m_immunosuppression %in% c("Yes", "No") &
      is_unchecked(m_immunosup_conditions___4) &
      mhccster == "No") ~ 0,

    (mhrheumd == "Yes" & (
      is_checked(m_rheum_conditions___1) |
      is_checked(m_rheum_conditions___2) |
      is_checked(m_rheum_conditions___3) |
      is_checked(m_rheum_conditions___4) |
      is_checked(m_rheum_conditions___5) |
      is_checked(m_rheum_conditions___6) |
      is_checked(m_rheum_conditions___7) |
      is_checked(m_rheum_conditions___88))
      ) &
    ((m_immunosuppression == "Yes" &
      is_checked(m_immunosup_conditions___4)) |
      mhccster == "Yes") ~ 1,

    is_unknown(mhrheumd) | is_unknown(m_immunosuppression) | is_unknown(mhccster) ~ 99
  )
}
