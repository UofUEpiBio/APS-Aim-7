## -----------------------------------------------------------------------------
## Organ Failure Trajectory (Systematic DAG)
## -----------------------------------------------------------------------------

# HELPER FUNCTION: Calculate total SOFA score for any day
# - 6 systems components of SOFA score, each 0-4.
# - Total SOFA score is sum of 6 sub scores (0-24)
calc_sofa_total_helper <- function(
  daily_pa02_lowest,
  daily_resp_lowest_pao2,
  daily_fio2_lowest_pao2,
  daily_o2_lowest_pao2,
  daily_spo2_lowest,
  daily_resp_lowest,
  daily_fio2_lowest,
  daily_o2_lowest,
  daily_platelet_8a,
  daily_tbili_8a,
  daily_sbp_8a,
  daily_dbp_8a,
  daily_dopa_dose_8a_mcg,
  daily_dopa_dos_8a_mcgkg,
  daily_dobuta_8a_mcg,
  daily_dobuta_8a_mcgkg,
  daily_epi_dose_8a_mcg,
  daily_epi_dose_8a_mcgkg,
  daily_ne_dose_8a_mcg,
  daily_ne_dose_8a_mcgkg,
  m_weight_kg,
  daily_gcs_8a,
  daily_cr_8a
) {

  ## Calculate P/F and S/F ratios for respiratory component
  pf_ratio <- calc_pf_ratio(
    low_pao2 = daily_pa02_lowest,
    resp_low_pao2 = daily_resp_lowest_pao2,
    fio2_low_pao2 = daily_fio2_lowest_pao2,
    o2_low_pao2 = daily_o2_lowest_pao2
  )

  sf_ratio <- calc_sf_ratio(
    low_spo2 = daily_spo2_lowest,
    resp_low_spo2 = daily_resp_lowest,
    fio2_low_spo2 = daily_fio2_lowest,
    o2_low_spo2 = daily_o2_lowest
  )

  ## Calculate individual SOFA component scores
  sofa_resp <- calc_sofa_resp(pf_ratio = pf_ratio, sf_ratio = sf_ratio)
  sofa_coag <- calc_sofa_coag(platelets = daily_platelet_8a)
  sofa_livr <- calc_sofa_livr(bilirubin = daily_tbili_8a)
  sofa_card <- calc_sofa_card(
    sbp = daily_sbp_8a,
    dbp = daily_dbp_8a,
    dopa_mcg = daily_dopa_dose_8a_mcg,
    dopa_mcgkg = daily_dopa_dos_8a_mcgkg,
    dobu_mcg = daily_dobuta_8a_mcg,
    dobu_mcgkg = daily_dobuta_8a_mcgkg,
    epin_mcg = daily_epi_dose_8a_mcg,
    epin_mcgkg = daily_epi_dose_8a_mcgkg,
    nore_mcg = daily_ne_dose_8a_mcg,
    nore_mcgkg = daily_ne_dose_8a_mcgkg,
    weight_kg = m_weight_kg
  )
  sofa_cns <- calc_sofa_cns(gcs = daily_gcs_8a)
  sofa_rena <- calc_sofa_rena(cr = daily_cr_8a)

  ## Sum all components to get total SOFA score
  # QUESTION: How to handle NAs? Many fields have one ore more NA and therefore lead to NA total SOFA.
  # - Some are due to how Matt has written his support functions. Should we rely on these?
  total_sofa <- rowSums(
    cbind(sofa_resp, sofa_coag, sofa_livr, sofa_card, sofa_cns, sofa_rena),
    na.rm = FALSE
  )

  return(total_sofa)
}


#' Calculate the systematic DAG variable for total SOFA score on Day 0
#'
#' `calc_sys_sofa_total_0` calculates the total SOFA score on Day 0.
#' This score assesses organ dysfunction across six organ systems using worst
#' values within the first 24 hours.
#'
#' @param daily_pa02_lowest_0 Numeric vector. Lowest PaO2 on Day 0.
#' @param daily_resp_lowest_pao2_0 Character vector. Respiratory support at lowest PaO2.
#' @param daily_fio2_lowest_pao2_0 Numeric vector. FiO2 at lowest PaO2.
#' @param daily_o2_lowest_pao2_0 Numeric vector. Oxygen flow at lowest PaO2.
#' @param daily_spo2_lowest_0 Numeric vector. Lowest SpO2 on Day 0.
#' @param daily_resp_lowest_0 Character vector. Respiratory support at lowest SpO2.
#' @param daily_fio2_lowest_0 Numeric vector. FiO2 at lowest SpO2.
#' @param daily_o2_lowest_0 Numeric vector. Oxygen flow at lowest SpO2.
#' @param daily_platelet_8a_0 Numeric vector. Platelet count on Day 0.
#' @param daily_tbili_8a_0 Numeric vector. Total bilirubin on Day 0.
#' @param daily_sbp_8a_0 Numeric vector. Systolic BP on Day 0.
#' @param daily_dbp_8a_0 Numeric vector. Diastolic BP on Day 0.
#' @param daily_dopa_dose_8a_0_mcg Numeric vector. Dopamine dose (mcg/min).
#' @param daily_dopa_dose_8a_0_mcgkg Numeric vector. Dopamine dose (mcg/min/kg).
#' @param daily_dobuta_8a_0_mcg Numeric vector. Dobutamine dose (mcg/min).
#' @param daily_dobuta_8a_0_mcgkg Numeric vector. Dobutamine dose (mcg/min/kg).
#' @param daily_epi_dose_8a_0_mcg Numeric vector. Epinephrine dose (mcg/min).
#' @param daily_epi_dose_8a_0_mcgkg Numeric vector. Epinephrine dose (mcg/min/kg).
#' @param daily_ne_dose_8a_0_mcg Numeric vector. Norepinephrine dose (mcg/min).
#' @param daily_ne_dose_8a_0_mcgkg Numeric vector. Norepinephrine dose (mcg/min/kg).
#' @param m_weight_kg Numeric vector. Patient weight in kg.
#' @param daily_gcs_8a_0 Character vector. Glasgow Coma Score on Day 0.
#' @param daily_cr_8a_0 Numeric vector. Creatinine on Day 0.
#'
#' @returns A numeric vector representing the total SOFA score on Day 0 (range 0-24),
#' or NA if component values are missing.
#' @export
calc_sys_sofa_total_0 <- function(
  daily_pa02_lowest_0,
  daily_resp_lowest_pao2_0,
  daily_fio2_lowest_pao2_0,
  daily_o2_lowest_pao2_0,
  daily_spo2_lowest_0,
  daily_resp_lowest_0,
  daily_fio2_lowest_0,
  daily_o2_lowest_0,
  daily_platelet_8a_0,
  daily_tbili_8a_0,
  daily_sbp_8a_0,
  daily_dbp_8a_0,
  daily_dopa_dose_8a_0_mcg,
  daily_dopa_dose_8a_0_mcgkg,
  daily_dobuta_8a_0_mcg,
  daily_dobuta_8a_0_mcgkg,
  daily_epi_dose_8a_0_mcg,
  daily_epi_dose_8a_0_mcgkg,
  daily_ne_dose_8a_0_mcg,
  daily_ne_dose_8a_0_mcgkg,
  m_weight_kg,
  daily_gcs_8a_0,
  daily_cr_8a_0
) {

  ## Use helper function to calculate SOFA score
  calc_sofa_total_helper(
    daily_pa02_lowest = daily_pa02_lowest_0,
    daily_resp_lowest_pao2 = daily_resp_lowest_pao2_0,
    daily_fio2_lowest_pao2 = daily_fio2_lowest_pao2_0,
    daily_o2_lowest_pao2 = daily_o2_lowest_pao2_0,
    daily_spo2_lowest = daily_spo2_lowest_0,
    daily_resp_lowest = daily_resp_lowest_0,
    daily_fio2_lowest = daily_fio2_lowest_0,
    daily_o2_lowest = daily_o2_lowest_0,
    daily_platelet_8a = daily_platelet_8a_0,
    daily_tbili_8a = daily_tbili_8a_0,
    daily_sbp_8a = daily_sbp_8a_0,
    daily_dbp_8a = daily_dbp_8a_0,
    daily_dopa_dose_8a_mcg = daily_dopa_dose_8a_0_mcg,
    daily_dopa_dos_8a_mcgkg = daily_dopa_dose_8a_0_mcgkg,
    daily_dobuta_8a_mcg = daily_dobuta_8a_0_mcg,
    daily_dobuta_8a_mcgkg = daily_dobuta_8a_0_mcgkg,
    daily_epi_dose_8a_mcg = daily_epi_dose_8a_0_mcg,
    daily_epi_dose_8a_mcgkg = daily_epi_dose_8a_0_mcgkg,
    daily_ne_dose_8a_mcg = daily_ne_dose_8a_0_mcg,
    daily_ne_dose_8a_mcgkg = daily_ne_dose_8a_0_mcgkg,
    m_weight_kg = m_weight_kg,
    daily_gcs_8a = daily_gcs_8a_0,
    daily_cr_8a = daily_cr_8a_0
  )
}


#' Calculate the systematic DAG variable for total SOFA score on Day -1
#'
#' `calc_sys_sofa_total_m1` calculates the total SOFA score on Day -1.
#' This score assesses organ dysfunction across six organ systems using worst
#' values from the day prior to enrollment.
#'
#' @param dailysofa_perf_m1 Character vector. The `dailysofa_perf_m1` column from the data.
#' @param daily_pa02_lowest_m1 Numeric vector. Lowest PaO2 on Day -1.
#' @param daily_resp_lowest_pao2_m1 Character vector. Respiratory support at lowest PaO2.
#' @param daily_fio2_lowest_pao2_m1 Numeric vector. FiO2 at lowest PaO2.
#' @param daily_o2_lowest_pao2_m1 Numeric vector. Oxygen flow at lowest PaO2.
#' @param daily_spo2_lowest_m1 Numeric vector. Lowest SpO2 on Day -1.
#' @param daily_resp_lowest_m1 Character vector. Respiratory support at lowest SpO2.
#' @param daily_fio2_lowest_m1 Numeric vector. FiO2 at lowest SpO2.
#' @param daily_o2_lowest_m1 Numeric vector. Oxygen flow at lowest SpO2.
#' @param daily_platelet_8a_m1 Numeric vector. Platelet count on Day -1.
#' @param daily_tbili_8a_m1 Numeric vector. Total bilirubin on Day -1.
#' @param daily_sbp_8a_m1 Numeric vector. Systolic BP on Day -1.
#' @param daily_dbp_8a_m1 Numeric vector. Diastolic BP on Day -1.
#' @param daily_dopa_dose_8a_m1_mcg Numeric vector. Dopamine dose (mcg/min).
#' @param daily_dopa_dos_8a_m1_mcgkg Numeric vector. Dopamine dose (mcg/min/kg).
#' @param daily_dobuta_8a_m1_mcg Numeric vector. Dobutamine dose (mcg/min).
#' @param daily_dobuta_8a_m1_mcgkg Numeric vector. Dobutamine dose (mcg/min/kg).
#' @param daily_epi_dose_8a_m1_mcg Numeric vector. Epinephrine dose (mcg/min).
#' @param daily_epi_dose_8a_m1_mcgkg Numeric vector. Epinephrine dose (mcg/min/kg).
#' @param daily_ne_dose_8a_m1_mcg Numeric vector. Norepinephrine dose (mcg/min).
#' @param daily_ne_dose_8a_m1_mcgkg Numeric vector. Norepinephrine dose (mcg/min/kg).
#' @param m_weight_kg Numeric vector. Patient weight in kg.
#' @param daily_gcs_8a_m1 Character vector. Glasgow Coma Score on Day -1.
#' @param daily_cr_8a_m1 Numeric vector. Creatinine on Day -1.
#'
#' @returns A numeric vector representing the total SOFA score on Day -1 (range 0-24),
#' 99 if not applicable (patient not in hospital), or NA if component values are missing.
#' @export
calc_sys_sofa_total_m1 <- function(
  dailysofa_perf_m1,
  daily_pa02_lowest_m1,
  daily_resp_lowest_pao2_m1,
  daily_fio2_lowest_pao2_m1,
  daily_o2_lowest_pao2_m1,
  daily_spo2_lowest_m1,
  daily_resp_lowest_m1,
  daily_fio2_lowest_m1,
  daily_o2_lowest_m1,
  daily_platelet_8a_m1,
  daily_tbili_8a_m1,
  daily_sbp_8a_m1,
  daily_dbp_8a_m1,
  daily_dopa_dose_8a_m1_mcg,
  daily_dopa_dos_8a_m1_mcgkg,
  daily_dobuta_8a_m1_mcg,
  daily_dobuta_8a_m1_mcgkg,
  daily_epi_dose_8a_m1_mcg,
  daily_epi_dose_8a_m1_mcgkg,
  daily_ne_dose_8a_m1_mcg,
  daily_ne_dose_8a_m1_mcgkg,
  m_weight_kg,
  daily_gcs_8a_m1,
  daily_cr_8a_m1
) {

  ## Check if patient was not in hospital on Day -1
  ## If not applicable, return 99
  not_applicable <- dailysofa_perf_m1 == "Not Available"

  ## Use helper function to calculate SOFA score
  total_sofa <- calc_sofa_total_helper(
    daily_pa02_lowest = daily_pa02_lowest_m1,
    daily_resp_lowest_pao2 = daily_resp_lowest_pao2_m1,
    daily_fio2_lowest_pao2 = daily_fio2_lowest_pao2_m1,
    daily_o2_lowest_pao2 = daily_o2_lowest_pao2_m1,
    daily_spo2_lowest = daily_spo2_lowest_m1,
    daily_resp_lowest = daily_resp_lowest_m1,
    daily_fio2_lowest = daily_fio2_lowest_m1,
    daily_o2_lowest = daily_o2_lowest_m1,
    daily_platelet_8a = daily_platelet_8a_m1,
    daily_tbili_8a = daily_tbili_8a_m1,
    daily_sbp_8a = daily_sbp_8a_m1,
    daily_dbp_8a = daily_dbp_8a_m1,
    daily_dopa_dose_8a_mcg = daily_dopa_dose_8a_m1_mcg,
    daily_dopa_dos_8a_mcgkg = daily_dopa_dos_8a_m1_mcgkg,
    daily_dobuta_8a_mcg = daily_dobuta_8a_m1_mcg,
    daily_dobuta_8a_mcgkg = daily_dobuta_8a_m1_mcgkg,
    daily_epi_dose_8a_mcg = daily_epi_dose_8a_m1_mcg,
    daily_epi_dose_8a_mcgkg = daily_epi_dose_8a_m1_mcgkg,
    daily_ne_dose_8a_mcg = daily_ne_dose_8a_m1_mcg,
    daily_ne_dose_8a_mcgkg = daily_ne_dose_8a_m1_mcgkg,
    m_weight_kg = m_weight_kg,
    daily_gcs_8a = daily_gcs_8a_m1,
    daily_cr_8a = daily_cr_8a_m1
  )

  ## Return 99 if not applicable, otherwise return total SOFA
  dplyr::if_else(not_applicable, 99, total_sofa, missing = NA_real_)
}
