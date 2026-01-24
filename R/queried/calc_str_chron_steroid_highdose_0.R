## -----------------------------------------------------------------------------
## Chronic Steroids High Dose (Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate streamlined DAG chronic high-dose steroid use on Day 0
#'
#' Calculates the streamlined DAG variable for chronic high-dose steroid use
#' from baseline medical history data. Identifies patients with chronic systemic
#' corticosteroid use documented in immunosuppression history.
#'
#' @inheritParams day0_baseline_medical_history_params
#'
#' @returns Integer vector with values:
#' - `0` = No chronic high-dose steroid use
#' - `1` = Chronic high-dose steroid use present
#' - `99` = Unknown
#' @export
calc_str_chron_steroid_highdose_0 <- function(
  m_immunosuppression,
  m_immunosup_conditions___1
) {
  dplyr::case_when(
    m_immunosuppression %in% c("Yes", "No") & is_unchecked(m_immunosup_conditions___1) ~ 0,
    m_immunosuppression == "Yes" & is_checked(m_immunosup_conditions___1) ~ 1,
    is_unknown(m_immunosuppression) ~ 99
  )
}
