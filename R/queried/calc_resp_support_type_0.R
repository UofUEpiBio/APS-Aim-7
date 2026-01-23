## -----------------------------------------------------------------------------
## Respiratory Failure Severity (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the support type component of the systematic DAG variable for
#' respiratory failure severity on Day 0
#'
#' `calc_resp_support_type_0` calculates the respiratory support type
#' component of the systematic DAG variable for respiratory failure severity
#' from the data.
#'
#' @param daily_resp_8a_0_code Integer vector. The `daily_resp_8a_0_code` code map from the
#' `daily_resp_8a_0` column from the data.
#' @param dailysofa_perf_0 Character vector. The `dailysofa_perf_0` column from the data.
#' @param daily_standard_flow_8a_0 Numeric vector. The `daily_standard_flow_8a_0` column from the data.
#' @param daily_hfnc_fi02_8a_0 Numeric vector. The `daily_hfnc_fi02_8a_0` column from the data.
#' @param daily_niv_fi02_8a_0 Numeric vector. The `daily_niv_fi02_8a_0` column from the data.
#' @param daily_imv_fio2_8a_0 Numeric vector. The `daily_imv_fio2_8a_0` column from the data.
#' @param daily_epap_8a_0 Numeric vector. The `daily_epap_8a_0` column from the data.
#'
#' @returns A vector with values:
#' - 1 = No respiratory support on day 0
#' - 2 = Standard flow
#' - 3 = HFNC
#' - 4 = NIV
#' - 5 = IMV with PEEP < 12 but no ECMO
#' - 6 = IMV with PEEP ≥ 12 or ECMO
#' - 99 = Unknown
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

#' Calculate the S/F ratio component of the systematic DAG variable for
#' respiratory failure severity on Day 0
#'
#' `calc_sfratio_8a_0` calculates the S/F ratio component of the systematic
#' DAG variable for respiratory failure severity from the data.
#'
#' @param resp_support_type_0 Integer vector. The `resp_support_type_0` column from the data.
#' @param daily_spo2_8a_0 Numeric vector. The `daily_spo2_8a_0` column from the data.
#' @inheritParams calc_resp_support_type_0
#'
#' @returns A numeric vector representing the S/F ratio on day 0 based on
#' `resp_support_type_0`:
#' - 1 = [8a SpO2]/[0.21)
#' - 2 = [8a SpO2]/[0.21 + (0.03 * 8a O2 flow)
#' - 3, 4, 5, 6 = [8a SpO2]/[8a FiO2]
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
