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
  # ANSWERED: How to handle NAs? Many fields have one or more NA and therefore lead to NA total SOFA.
  # - Some are due to how Matt has written his support functions. Should we rely on these?
  # - ANSWER: Work with Matt to make these more robust (and make sure the data is there!), but otherwise assign 0 for a missing SOFA
  # - There is absolutely no way we can't calculate SOFA for Day 0, so NA should be exceedingly rare. Data should be there, should always be there! We shouldn't be assuming 0 SOFA points unless we absolutely have to.
  # - CC Ithan and Dan on emails to Matt
  total_sofa <- rowSums(
    cbind(sofa_resp, sofa_coag, sofa_livr, sofa_card, sofa_cns, sofa_rena),
    na.rm = FALSE
  )

  return(total_sofa)
}


# Calculate SYSTEMATIC DAG 'Organ Failure Trajectory: Total SOFA Score Day 0'
#
# Value: Total SOFA score (range 0-24)
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


# Calculate SYSTEMATIC DAG 'Organ Failure Trajectory: Total SOFA Score Day -1'
#
# Value: Total SOFA score (range 0-24, or 99 if not applicable)
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


# Convenience wrapper function
# Returns a data frame with record_id, sofa_total_day_0, and sofa_total_day_m1 columns (one row per record_id)
wrapper_calc_sys_organ_failure_trajectory <- function(data) {
  data_with_sofa <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    # Remove the weight variable and merge it back in from the "Day 0" event
    # where weight is collected
    select(-m_weight_kg) |>
    left_join(data |>
      filter(event_label == 'Day 0') |>
      select(record_id, m_weight_kg),
      by='record_id') |>
    mutate(
      sofa_total_day_0 = calc_sys_sofa_total_0(
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
        daily_dopa_dos_8a_0_mcgkg,
        daily_dobuta_8a_0_mcg,
        daily_dobuta_8a_0_mcgkg,
        daily_epi_dose_8a_0_mcg,
        daily_epi_dose_8a_0_mcgkg,
        daily_ne_dose_8a_0_mcg,
        daily_ne_dose_8a_0_mcgkg,
        m_weight_kg,
        daily_gcs_8a_0,
        daily_cr_8a_0
      ),
      sofa_total_day_m1 = calc_sys_sofa_total_m1(
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
      )
    ) |>
    select(record_id, sofa_total_day_0, sofa_total_day_m1)

  data |>
    distinct(record_id) |>
    left_join(data_with_sofa, by = 'record_id')
}
