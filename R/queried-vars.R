## -----------------------------------------------------------------------------
## Neuromuscular Blockade (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for neuromuscular blockade on Day 0
#'
#' `calc_str_nmblockade_0` calculates the streamlined DAG variable for
#' neuromuscular blockade from the data. This variable represents daily
#' neuromuscular blocking agent use on admission day.
#'
#' @param daily_paralysis_0 Character vector. The `daily_paralysis_0` column
#' from the data.
#' @param trx_0 Character vector. The `trx_0` column from the data.
#'
#' @returns A vector with values:
#' - 0 = No neuromuscular blockade on day 0 or missing
#' - 1 = Neuromuscular blockade given on day 0
#' @export
calc_str_nmblockade_0 <- function(daily_paralysis_0, trx_0) {
  dplyr::case_when(
    daily_paralysis_0 == 'Administered' ~ 1,
    trx_0 == 'Available' & !is.na(daily_paralysis_0) ~ 0,
    trx_0 == 'Not Available' & is.na(daily_paralysis_0) ~ 0
  )
}

## -----------------------------------------------------------------------------
## Inflammatory Profile (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for inflammatory profile on Day 0
#'
#' `calc_str_inflamprofile_0` calculates the streamlined DAG variable for
#' inflammatory profile from the data. This variable represents
#' CRP level on admission day.
#'
#' @param daily_crp_8a_0 Numeric vector. The `daily_crp_8a_0` column from the data.
#' @param daily_crp_nc_0 Character vector. The `daily_crp_nc_0` column from the data.
#'
#' @returns A vector with values:
#' - 0 = Day 0 CRP not checked
#' - 1 = Day 0 CRP checked and < 15
#' - 2 = Day 0 CRP checked and ≥ 15
#' @export
calc_str_inflamprofile_0 <- function(daily_crp_8a_0, daily_crp_nc_0) {
  calc_crp_0(daily_crp_8a_0, daily_crp_nc_0)
}


# Calculate the CRP component of the inflammatory profile variable
calc_crp_0 <- function(daily_crp_8a_0, daily_crp_nc_0) {
  dplyr::case_when(
    daily_crp_8a_0 >= 15 ~ 2,
    daily_crp_8a_0 < 15 ~ 1,
    daily_crp_nc_0 == 'Not Collected' ~ 0
  )
}

# Calculate the Ferritin component of the inflammatory profile variable
calc_ferritin_0 <- function(daily_ferritin_8a_0, daily_ferritin_nc_0) {
  dplyr::case_when(
    daily_ferritin_8a_0 >= 700 ~ 2,
    daily_ferritin_8a_0 < 700 ~ 1,
    daily_ferritin_nc_0 == 'Not Collected' ~ 0
  )
}

# Calculate the Fibrinogen component of the inflammatory profile variable
calc_fibrinogen_0 <- function(daily_fibrinogen_8a_0, daily_fibrinogen_nc_0) {
  dplyr::case_when(
    daily_fibrinogen_8a_0 >= 150 ~ 2,
    daily_fibrinogen_8a_0 < 150 ~ 1,
    daily_fibrinogen_nc_0 == 'Not Collected' ~ 0
  )
}

# Calculate the D-dimer component of the inflammatory profile variable
calc_ddimer_0 <- function(daily_ddimer_8a_0, daily_ddimer_nc_0) {
  dplyr::case_when(
    daily_ddimer_8a_0 >= 1500 ~ 2,
    daily_ddimer_8a_0 < 1500 ~ 1,
    daily_ddimer_nc_0 == 'Not Collected' ~ 0
  )
}


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
    (daily_resp_8a_0_code == 7 | dailysofa_perf_0 == 'Not Available') ~ 1
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


## -----------------------------------------------------------------------------
## Chronic Moderate-Dose Steroid Use (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable for chronic moderate-dose steroid use on Day 0
#'
#' `calc_str_chron_steroid_moddose_0` calculates the systematic DAG variable for
#' chronic moderate-dose steroid use. This variable represents chronic
#' moderate-dose steroid use leading to possible adrenal insufficinecy (but not
#' definite adrenal insufficiency).
#'
#' @param mhccster Character vector. The `mhccster` column from the data.
#' @param m_endo_conditions___2 Character vector. The `m_endo_conditions___2` column
#' from the data.
#' @param m_immunosup_conditions___1 Character vector. The `m_immunosup_conditions___1`
#' column from the data.
#' @param m_endocrine Character vector. The `m_endocrine` column from the data.
#' @param m_immunosuppression Character vector. The `m_immunosuppression` column from the data.
#'
#' @returns A vector with values:
#' - 0 = No possible adrenal insufficiency
#' - 1 = Possible adrenal insufficiency
#' - 99 = Unknown
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


#' Calculate the streamlined DAG variable for chronic moderate-dose steroid use on Day 0
#'
#' `calc_str_chron_steroid_moddose_0` calculates the streamlined DAG variable for
#' chronic moderate-dose steroid use. This variable represents chronic
#' moderate-dose steroid use leading to possible adrenal insufficinecy (but not
#' definite adrenal insufficiency).
#'
#' @inheritParams calc_sys_chron_steroid_moddose_0
#'
#' @inherits calc_sys_chron_steroid_moddose_0 returns
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
