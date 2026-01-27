## -----------------------------------------------------------------------------
## Respiratory Failure Severity (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Respiratory Failure Severity: Respiratory Support Type' for Day 0
#
# Values:
# - 1 = No respiratory support
# - 2 = Standard flow oxygen
# - 3 = High-flow nasal cannula (HFNC)
# - 4 = Non-invasive ventilation (NIV)
# - 5 = Invasive mechanical ventilation (IMV) with PEEP < 12 cm H2O, no ECMO
# - 6 = IMV with PEEP ≥ 12 cm H2O, or ECMO
# - 99 = Unknown
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

# Calculate SYSTEMATIC DAG 'Respiratory Failure Severity: S/F Ratio' for Day 0
#
# Value: SpO2/FiO2 ratio calculated as:
# - Resp Support Type 1 (no support): SpO2 / 0.21
# - Resp Support Type 2 (standard flow): SpO2 / (0.21 + 0.03 × O2 flow rate)
# - Resp Support Types 3-6 (HFNC/NIV/IMV): SpO2 / FiO2
calc_sfratio_8a_0 <- function(
  resp_support_type_0, # Calculated by calc_resp_support_type_0()
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
