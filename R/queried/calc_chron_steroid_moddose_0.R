## -----------------------------------------------------------------------------
## Chronic Moderate-Dose Steroid Use (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate systematic DAG chronic moderate-dose steroid use on Day 0
#'
#' Calculates the systematic DAG variable for chronic moderate-dose steroid use
#' from medication history and baseline medical history. Represents chronic
#' moderate-dose steroid use leading to possible (but not definite) adrenal
#' insufficiency.
#'
#' @inheritParams day0_medications_params
#' @inheritParams day0_baseline_medical_history_params
#'
#' @returns Integer vector with values:
#' - `0` = No possible adrenal insufficiency
#' - `1` = Possible adrenal insufficiency (moderate-dose steroids without confirmed AI)
#' - `99` = Unknown
#' @export
calc_sys_chron_steroid_moddose_0 <- function(
  mhccster,
  m_endo_conditions___2,
  m_immunosup_conditions___1,
  m_endocrine,
  m_immunosuppression
) {
  dplyr::case_when(
    mhccster == "No" |
        (m_endocrine == "Yes" & is_checked(m_endo_conditions___2)) |
        (m_immunosuppression == "Yes" & is_checked(m_immunosup_conditions___1)) ~ 0,

    mhccster == "Yes" &
        m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) &
        m_immunosuppression %in% c("Yes", "No") & is_unchecked(m_immunosup_conditions___1) ~ 1,

    is_unknown(mhccster) | is_unknown(m_endocrine) | is_unknown(m_immunosuppression) ~ 99
  )
}


#' Calculate streamlined DAG chronic moderate-dose steroid use on Day 0
#'
#' Calculates the streamlined DAG variable for chronic moderate-dose steroid use
#' from medication history and baseline medical history. Simplified version that
#' excludes immunosuppression checkbox screening. Represents chronic moderate-dose
#' steroid use leading to possible (but not definite) adrenal insufficiency.
#'
#' @inheritParams day0_medications_params
#' @inheritParams day0_baseline_medical_history_params
#'
#' @inherit calc_sys_chron_steroid_moddose_0 return
#' @export
calc_str_chron_steroid_moddose_0 <- function(
  mhccster,
  m_endo_conditions___2,
  m_endocrine
) {
  dplyr::case_when(
    mhccster == "No" |
        (m_endocrine == "Yes" & is_checked(m_endo_conditions___2)) ~ 0,

    mhccster == "Yes" &
        m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) ~ 1,

    is_unknown(mhccster) | is_unknown(m_endocrine) ~ 99
  )
}
