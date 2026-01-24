## -----------------------------------------------------------------------------
## Hyperglycemia (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate systematic DAG hyperglycemia history on Day 0
#'
#' Calculates the systematic DAG variable 1 for hyperglycemia from baseline
#' medical history. Identifies patients with documented diabetes mellitus history.
#'
#' @inheritParams day0_baseline_medical_history_params
#'
#' @returns Integer vector with values:
#' - `0` = No diabetes medical history
#' - `1` = Diabetes medical history present
#' - `99` = Unknown
#' @export
calc_sys_hyperglyc_hist_0 <- function(
  m_endocrine,
  m_endo_conditions___1
) {
  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___1) ~ 0,
    m_endocrine == "Yes" & is_checked(m_endo_conditions___1) ~ 1,
    is_unknown(m_endocrine) ~ 99
  )
}


#' Calculate systematic DAG hyperglycemia glucose level on Day 0
#'
#' Calculates the systematic DAG variable 2 for hyperglycemia from daily
#' assessment data. Returns the measured glucose level or -1 if not collected.
#'
#' @inheritParams daily_assessment_params
#'
#' @returns Numeric vector representing glucose level in mg/dL, or -1 if not
#'   collected.
#' @export
calc_sys_hyperglyc_gluc_0 <- function(
  daily_gluc_8a_0,
  daily_gluc_nc_0
) {
  dplyr::case_when(
    !is.na(daily_gluc_8a_0) ~ daily_gluc_8a_0,
    daily_gluc_nc_0 == 'Not Collected' ~ -1
  )
}

#' Calculate streamlined DAG hyperglycemia on Day 0
#'
#' Calculates the streamlined DAG variable for hyperglycemia from daily
#' assessment data. Dichotomizes glucose at threshold of 250 mg/dL.
#'
#' @inheritParams daily_assessment_params
#'
#' @returns Integer vector with values:
#' - `0` = No hyperglycemia (glucose ≤ 250 mg/dL or not collected)
#' - `1` = Hyperglycemia present (glucose > 250 mg/dL)
#' @export
calc_str_hyperglyc_hist_0 <- function(
  daily_gluc_8a_0,
  daily_gluc_nc_0
) {
  dplyr::case_when(
    daily_gluc_8a_0 > 250 ~ 1,
    is.na(daily_gluc_8a_0) | (daily_gluc_8a_0 <= 250) ~ 0,
    daily_gluc_nc_0 == 'Not Collected' ~ 0
  )
}
