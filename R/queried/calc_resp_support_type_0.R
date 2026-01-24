## -----------------------------------------------------------------------------
## Respiratory Failure Severity (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate respiratory support type on Day 0
#'
#' Calculates the respiratory support type component of the systematic DAG
#' variable for respiratory failure severity from daily assessment data.
#' Categorizes the type and intensity of respiratory support on admission day.
#'
#' @inheritParams daily_assessment_params code_map_derived_params
#'
#' @returns Integer vector with values:
#' - `1` = No respiratory support on Day 0
#' - `2` = Standard flow oxygen
#' - `3` = High-flow nasal cannula (HFNC)
#' - `4` = Non-invasive ventilation (NIV)
#' - `5` = Invasive mechanical ventilation (IMV) with PEEP < 12 cm H2O, no ECMO
#' - `6` = IMV with PEEP ≥ 12 cm H2O, or ECMO
#' - `99` = Unknown
#' @export
calc_resp_support_type_0 <- function(
  daily_resp_8a_0_code,
  dailysofa_perf_0,
  daily_standard_flow_8a_0,
  daily_hfnc_fi02_8a_0,
  daily_niv_fi02_8a_0,
  daily_imv_fio2_8a_0,
  daily_epap_8a_0
) {

  dplyr::case_when(
    # ECMO
    daily_resp_8a_0_code %in% c(1, 2) ~ 6,
    # IMV with PEEP >= 12 (same code as ECMO)
    daily_resp_8a_0_code == 3 & !is.na(daily_imv_fio2_8a_0) & daily_epap_8a_0 >= 12 ~ 6,
    # IMV with PEEP < 12
    daily_resp_8a_0_code == 3 & !is.na(daily_imv_fio2_8a_0) & daily_epap_8a_0 < 12 ~ 5,
    # NIV
    (daily_resp_8a_0_code == 4) & !is.na(daily_niv_fi02_8a_0) ~ 4,
    # HFNC
    (daily_resp_8a_0_code == 5) & !is.na(daily_hfnc_fi02_8a_0) ~ 3,
    # Standard flow
    (daily_resp_8a_0_code == 6) & !is.na(daily_standard_flow_8a_0) ~ 2,
    # No respiratory support on day 0 or not applicable
    (daily_resp_8a_0_code == 7 | dailysofa_perf_0 == 'Not Available') ~ 1,
    # Unknown
    daily_resp_8a_0_code == 99 ~ 99
  )
}

#' Calculate S/F ratio on Day 0
#'
#' Calculates the SpO2/FiO2 (S/F) ratio component of the systematic DAG variable
#' for respiratory failure severity from daily assessment data. The calculation
#' method varies based on respiratory support type.
#'
#' @inheritParams derived_respiratory_params
#' @inheritParams daily_assessment_params
#'
#' @returns Numeric vector representing the S/F ratio on Day 0, calculated as:
#' - For no respiratory support (type 1): SpO2 / 0.21
#' - For standard flow (type 2): SpO2 / (0.21 + 0.03 × O2 flow rate)
#' - For HFNC/NIV/IMV (types 3-6): SpO2 / FiO2
#' @export
calc_sfratio_8a_0 <- function(
  resp_support_type_0,
  daily_spo2_8a_0,
  daily_standard_flow_8a_0,
  daily_hfnc_fi02_8a_0,
  daily_niv_fi02_8a_0,
  daily_imv_fio2_8a_0
) {

  dplyr::case_when(
    resp_support_type_0 == 1 ~ daily_spo2_8a_0 / 0.21,
    resp_support_type_0 == 2 ~ daily_spo2_8a_0 / (0.21 + (0.03 * daily_standard_flow_8a_0)),
    resp_support_type_0 == 3 ~ daily_spo2_8a_0 / daily_hfnc_fi02_8a_0,
    resp_support_type_0 == 4 ~ daily_spo2_8a_0 / daily_niv_fi02_8a_0,
    resp_support_type_0 %in% c(5, 6) & !is.na(daily_imv_fio2_8a_0) ~ daily_spo2_8a_0 / daily_imv_fio2_8a_0
  )
}
