## -----------------------------------------------------------------------------
## Neuromuscular Blockade (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the systematic DAG variable for neuromuscular blockade on Day 0
#'
#' `calc_sys_nmblockade_0` calculates the systematic DAG variable for
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
calc_sys_nmblockade_0 <- function(daily_paralysis_0, trx_0) {
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


## -----------------------------------------------------------------------------
## Definite Adrenal Insufficiency (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the systematic DAG variable for adrenal insufficiency on Day 0
#'
#' `calc_sys_def_adrenal_insufficiency_0` calculates the systematic DAG variable for
#' adrenal insufficiency from the data.
#'
#' @param m_endocrine Character vector. The `m_endocrine` column from the data.
#' @param m_endo_conditions___2 Character vector. The `m_endo_conditions___2` column
#' from the data.
#' @param m_immunosuppression Character vector. The `m_immunosuppression` column from the data.
#' @param m_immunosup_conditions___1 Character vector. The `m_immunosup_conditions___1`
#' column from the data.
#'
#' @returns A vector with values:
#' - 0 = No definite adrenal insufficiency
#' - 1 = Definite adrenal insufficiency
#' - 99 = Unknown
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

#' Calculate the streamlined DAG variable for adrenal insufficiency on Day 0
#'
#' `calc_sys_def_adrenal_insufficiency_0` calculates the streamlined DAG variable for
#' adrenal insufficiency from the data.
#'
#' @inheritParams calc_sys_def_adrenal_insufficiency_0
#'
#' @inherits calc_sys_def_adrenal_insufficiency_0 returns
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


## -----------------------------------------------------------------------------
## Chronic Steroids High Dose (Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the streamlined DAG variable for chronic high-dose steroid use on Day 0
#'
#' `calc_str_chron_steroid_highdose_0` calculates the streamlined DAG variable for
#' chronic high-dose steroid use from the data.
#'
#' @param m_immunosup_conditions___1 Character vector. The `m_immunosup_conditions___1`
#' column from the data.
#'
#' @returns A vector with values:
#' - 0 = No chronic high-dose steroid use
#' - 1 = Chronic high-dose steroid use
#' - 99 = Unknown
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


## -----------------------------------------------------------------------------
## Steroid Responsive Comorbidity (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable for steroid responsive comorbidity on Day 0
#'
#' `calc_sys_steroid_comorb_0` calculates the systematic DAG variable for
#' steroid responsive comorbidity from the data.
#'
#' @param mhrheumd Character vector. The `mhrheumd` column from the data.
#' @param m_rheum_conditions___1 Character vector. The `m_rheum_conditions___1` column
#' from the data.
#' @param m_rheum_conditions___2 Character vector. The `m_rheum_conditions___2` column
#' from the data.
#' @param m_rheum_conditions___3 Character vector. The `m_rheum_conditions___3` column
#' from the data.
#' @param m_rheum_conditions___4 Character vector. The `m_rheum_conditions___4` column
#' from the data.
#' @param m_rheum_conditions___5 Character vector. The `m_rhe um_conditions___5` column
#' from the data.
#' @param m_rheum_conditions___6 Character vector. The `m_rheum_conditions___6` column
#' from the data.
#' @param m_rheum_conditions___7 Character vector. The `m_rheum_conditions___7` column
#' from the data.
#' @param m_rheum_conditions___8 Character vector. The `m_rheum_conditions___8` column
#' from the data.
#' @param m_rheum_conditions___88 Character vector. The `m_rheum_conditions___88` column
#' from the data.
#' @param m_pulmonary Character vector. The `m_pulmonary` column from the data.
#' @param m_pulm_conditions___6 Character vector. The `m_pulm_conditions___6` column
#' from the data.
#'
#' @returns A vector with values:
#' - 0 = No steroid responsive comorbidity
#' - 1 = Steroid responsive comorbidity present
#' - 99 = Unknown
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

      mhrheumd == "Unknown" | m_pulmonary == "Unknown" ~ 99
  )
}


#' Calculate the streamlined DAG variable for steroid responsive comorbidity on Day 0
#'
#' `calc_str_steroid_comorb_0` calculates the streamlined DAG variable for
#' steroid responsive comorbidity from the data.
#'
#' @inheritParams calc_sys_steroid_comorb_0
#'
#' @inherits calc_sys_steroid_comorb_0 returns
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


## -----------------------------------------------------------------------------
## Obstructive Lung Disease (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the streamlined DAG variable for obstructive lung disease on Day 0
#'
#' `calc_sys_obstruct_lung_0` calculates the streamlined DAG variable for
#' obstructive lung disease from the data.
#'
#' @param m_pulmonary Character vector. The `m_pulmonary` column from the data.
#' @param m_pulm_conditions___1 Character vector. The `m_pulm_conditions___1` column
#' from the data.
#' @param m_pulm_conditions___3 Character vector. The `m_pulm_conditions___3` column
#' from the data.
#'
#' @returns A vector with values:
#' - 0 = No obstructive lung disease
#' - 1 = Obstructive lung disease present
#' - 99 = Unknown
#' @export
calc_sys_obstruct_lung_0 <- function(
  m_pulmonary,
  m_pulm_conditions___1,
  m_pulm_conditions___3
) {
  dplyr::case_when(
    m_pulmonary == "Yes" &
      (is_checked(m_pulm_conditions___1) | is_checked(m_pulm_conditions___3)) ~ 1,
    m_pulmonary %in% c("Yes", "No") &
      (is_unchecked(m_pulm_conditions___1) & is_unchecked(m_pulm_conditions___3)) ~ 0,
    m_pulmonary == "Unknown" ~ 99
  )
}


## -----------------------------------------------------------------------------
## Chronic Immunocompromise (Systematic DAG)
## -----------------------------------------------------------------------------


#' Calculate the streamlined DAG variable for chronic immunocompromise on Day 0
#'
#' `calc_sys_chron_immunocomp_0` calculates the streamlined DAG variable for
#' chronic immunocompromise from the data.
#'
#' @param m_immunosuppression Character vector. The `m_immunosuppression` column from the data.
#' @param m_immunosup_conditions___2 Character vector. The `m_immunosup_conditions___2` column
#' from the data.
#' @param m_immunosup_conditions___3 Character vector. The `m_immunosup_conditions___3` column
#' from the data.
#' @param m_immunosup_conditions___4 Character vector. The `m_immunosup_conditions___4` column
#' from the data.
#' @param m_immunosup_conditions___5 Character vector. The `m_immunosup_conditions___5` column
#' from the data.
#' @param m_immunosup_conditions___6 Character vector. The `m_immunosup_conditions___6` column
#' from the data.
#' @param m_immunosup_conditions___7 Character vector. The `m_immunosup_conditions___7` column
#' from the data.
#' @param m_immunosup_conditions___88 Character vector. The `m_immunosup_conditions___88` column
#' from the data.
#'
#' @returns A vector with values:
#' - 0 = No chronic immunocompromise
#' - 1 = Chronic immunocompromise present
#' - 99 = Unknown
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

    m_immunosuppression == "Unknown" ~ 99
  )
}


## -----------------------------------------------------------------------------
## Hyperglycemia (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable 1 for hyperglycemia on Day 0
#'
#' `calc_sys_hyperglyc_hist_0` calculates the systematic DAG variable 1 for
#' hyperglycemia from the data.
#'
#' @param m_endocrine Character vector. The `m_endocrine` column from the data.
#' @param m_endo_conditions___1 Character vector. The `m_endo_conditions___1` column
#' from the data.
#'
#' @returns A vector with values:
#' - 0 = No diabetes medical history
#' - 1 = Diabetes medical history present
#' - 99 = Unknown
#' @export
calc_sys_hyperglyc_hist_0 <- function(
  m_endocrine,
  m_endo_conditions___1
) {
  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___1) ~ 0,
    m_endocrine == "Yes" & is_checked(m_endo_conditions___1) ~ 1,
    m_endocrine == "Unknown" ~ 99
  )
}


#' Calculate the systematic DAG variable 2 for hyperglycemia on Day 0
#'
#' `calc_sys_hyperglyc_gluc_0` calculates the systematic DAG variable 2 for
#' hyperglycemia from the data.
#'
#' @param daily_gluc_8a_0 Numeric vector. The `daily_gluc_8a_0` column from the data.
#' @param daily_gluc_nc_0 Character vector. The `daily_gluc_nc_0` column from the data.
#'
#' @returns A numeric vector representing the glucose level on day 0 based on
#' available data:
#' - Glucose value if measured
#' - -1 if glucose not collected
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

#' Calculate the streamlined DAG variable for hyperglycemia on Day 0
#'
#' `calc_str_hyperglyc_hist_0` calculates the streamlined DAG variable for
#' hyperglycemia from the data.
#'
#' @inheritParams calc_sys_hyperglyc_gluc_0
#'
#' @returns A vector with values:
#' - 0 = No hyperglycemia on day 0 (glucose ≤ 250 or not collected)
#' - 1 = Hyperglycemia present (glucose > 250)
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


## -----------------------------------------------------------------------------
## Active Fungal Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the systematic DAG variable for active fungal infection on Day 0
#'
#' `calc_sys_active_fungal_0` calculates the systematic DAG variable for
#' active fungal infection from the data.
#'
#' @param mhantifungals Character vector. The `mhantifungals` column from the data.
#' @param daily_antifungal_0 Character vector. The `daily_antifungal_0` column from the data.
#' @param trx_0 Character vector. The `trx_0` column from the data.
#'
#' @returns A vector with values:
#' - 0 = No active fungal infection
#' - 1 = Active fungal infection present
#' - 99 = Unknown
#' @export
calc_sys_active_fungal_0 <- function(
  mhantifungals,
  daily_antifungal_0,
  trx_0
) {
  dplyr::case_when(
    mhantifungals == "No" & (trx_0 == "Not Available" | daily_antifungal_0 == "Not administered") ~ 0,

    mhantifungals == "Yes" | (trx_0 == "Available" & daily_antifungal_0 == "Administered") ~ 1,

    is_unknown(mhantifungals) | daily_antifungal_0 == "UNK" ~ 99
  )
}


## -----------------------------------------------------------------------------
## Age (Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the streamlined DAG variable for age on Day 0
#'
#' `calc_age_0` calculates the streamlined DAG variable for age from the data.
#'
#' @param age Numeric vector. The `age` column from the data.
#'
#' @returns A numeric vector representing age on day 0.
#' @export
calc_str_age_0 <- function(
  age
) {
  dplyr::case_when(
    !is.na(age) & age >= 0 ~ age
  )
}
