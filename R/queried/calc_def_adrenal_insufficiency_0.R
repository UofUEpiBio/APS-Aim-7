## -----------------------------------------------------------------------------
## Definite Adrenal Insufficiency (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate systematic DAG definite adrenal insufficiency on Day 0
#'
#' Calculates the systematic DAG variable for adrenal insufficiency from baseline
#' medical history. Identifies patients with documented adrenal insufficiency or
#' chronic high-dose steroid use.
#'
#' @inheritParams day0_baseline_medical_history_params
#'
#' @returns Integer vector with values:
#' - `0` = No definite adrenal insufficiency
#' - `1` = Definite adrenal insufficiency present
#' - `99` = Unknown
#' @export
calc_sys_def_adrenal_insufficiency_0 <- function(
  m_endocrine,
  m_endo_conditions___2,
  m_immunosuppression,
  m_immunosup_conditions___1
  ) {

  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) & m_immunosuppression %in% c("Yes", "No") & is_unchecked(m_immunosup_conditions___1) ~ 0,

    (m_endocrine == "Yes" & is_checked(m_endo_conditions___2)) |
    (m_immunosuppression == "Yes" & is_checked(m_immunosup_conditions___1)) ~ 1,

    is_unknown(m_endocrine) | is_unknown(m_immunosuppression) ~ 99
  )
}

#' Calculate streamlined DAG definite adrenal insufficiency on Day 0
#'
#' Calculates the streamlined DAG variable for adrenal insufficiency from baseline
#' medical history. Simplified version that only checks endocrine conditions.
#'
#' @inheritParams day0_baseline_medical_history_params
#'
#' @inherit calc_sys_def_adrenal_insufficiency_0 return
#' @export
calc_str_adrenal_insufficiency_0 <- function(
  m_endocrine,
  m_endo_conditions___2
) {
  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) ~ 0,
    m_endocrine == "Yes" & is_checked(m_endo_conditions___2) ~ 1,
    is_unknown(m_endocrine) ~ 99
  )
}
