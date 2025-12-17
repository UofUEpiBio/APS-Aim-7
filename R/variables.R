## -----------------------------------------------------------------------------
## Global Physiology Severity (Systematic DAG)
## -----------------------------------------------------------------------------

calc_sys_global_phys_sev_0 <- function(
  # day0_vars
  vs_perf, # branching logic
  hightemp_vsorres,
  lowtemp_vsorres,
  highhr_vsorres,
  lowhr_vsorres,
  highrr_vsorres,
  lowrr_vsorres,
  lowsysbp_vsorres,
  lowdbp_vsorres,

  # hospitalform_vars
  ## - Day 0
  dailysofa_perf_0, # branching logic
  daily_resp_8a_0, # branching logic
  daily_imv_mode_8a_0,
  daily_imv_fio2_8a_0,
  daily_pao2_occur_0, # branching logic
  daily_pa02_lowest_0,
  daily_paco2_lowest_0,
  daily_ph_lowest_0,
  daily_k_nc_0, # branching logic
  daily_k_8a_0,
  daily_na_nc_0, # branching logic
  daily_na_8a_0,
  daily_hct_nc_0, # branching logic
  daily_hct_8a_0,
  daily_wbc_nc_0, # branching logic
  daily_wbc_8a_0,
  daily_cr_nc_0, # branching logic
  daily_cr_8a_0,
  daily_gcs_8a_0,

  ## - Day -1
  dailysofa_perf_m1, # branching logic
  daily_resp_8a_m1, # branching logic
  daily_imv_mode_8a_m1,
  daily_imv_fio2_8a_m1,
  daily_pao2_occur_m1, # branching logic
  daily_pa02_lowest_m1,
  daily_paco2_lowest_m1,
  daily_ph_lowest_m1,
  daily_k_nc_m1, # branching logic
  daily_k_8a_m1,
  daily_na_nc_m1, # branching logic
  daily_na_8a_m1,
  daily_hct_nc_m1, # branching logic
  daily_hct_8a_m1,
  daily_wbc_nc_m1, # branching logic
  daily_wbc_8a_m1,
  daily_cr_nc_m1, # branching logic
  daily_cr_8a_m1,
  daily_gcs_8a_m1,

  ## - Day -2
  dailysofa_perf_m2, # branching logic
  daily_resp_8a_m2, # branching logic
  daily_imv_mode_8a_m2,
  daily_imv_fio2_8a_m2,
  daily_pao2_occur_m2, # branching logic
  daily_pa02_lowest_m2,
  daily_paco2_lowest_m2,
  daily_ph_lowest_m2,
  daily_k_nc_m2, # branching logic
  daily_k_8a_m2,
  daily_na_nc_m2, # branching logic
  daily_na_8a_m2,
  daily_hct_nc_m2, # branching logic
  daily_hct_8a_m2,
  daily_wbc_nc_m2, # branching logic
  daily_wbc_8a_m2,
  daily_cr_nc_m2, # branching logic
  daily_cr_8a_m2,
  daily_gcs_8a_m2
) {

  # Helper function: Get value with lookback (Day 0 -> Day -1 -> Day -2)
  get_value_with_lookback <- function(val_0, val_m1, val_m2) {
    dplyr::coalesce(val_0, val_m1, val_m2)
  }

  # Score each component (0-4 points each)

  # 1. Temperature (rectal, °C) - use most extreme
  # - Get highest and lowest temp (`hightemp_vsorres` can be lower than `lowtemp_vsorres`)
  temp_high <- pmax(hightemp_vsorres, lowtemp_vsorres)
  temp_low <- pmin(hightemp_vsorres, lowtemp_vsorres)

  # - Calculate APS temperature points
  temp_points <- dplyr::case_when(
    temp_high >= 41 | temp_low < 30 ~ 4,
    temp_high >= 39 | temp_low < 32 ~ 3,
    temp_low < 34 ~ 2,
    temp_high >= 38.5 | temp_low < 36 ~ 1,
    temp_high >= 36 | temp_low < 38.5 ~ 0
  )

  # 2. Mean Arterial Pressure (mmHg)
  # - Calculate MAP from systolic and diastolic BP
  map <- (lowsysbp_vsorres + (2 * lowdbp_vsorres)) / 3

  # - Calculate APS MAP points
  map_points <- dplyr::case_when(
    map >= 160 ~ 4,
    map >= 130 ~ 3,
    map >= 110 ~ 2,
    map >= 70  ~ 0,
    map >= 50  ~ 2,
    map < 50   ~ 4
  )

  # 3. Heart Rate (bpm) - use most extreme
  # - Get highest and lowest heart rate (`highhr_vsorres` can be lower than `lowhr_vsorres`)
  hr_high <- pmax(highhr_vsorres, lowhr_vsorres)
  hr_low <- pmin(highhr_vsorres, lowhr_vsorres)

  # - Calculate APS heart rate points
  hr_points <- dplyr::case_when(
    hr_high >= 180 | hr_low < 40 ~ 4,
    hr_high >= 140 | hr_low < 55 ~ 3,
    hr_high >= 110 | hr_low < 70 ~ 2,
    hr_high >= 70 | hr_low < 110 ~ 0
  )

  # 4. Respiratory Rate (breaths/min) - use worst (most extreme)
  # - Get highest and lowest respiratory rate (`highrr_vsorres` can be lower than `lowrr_vsorres`)
  rr_high <- pmax(highrr_vsorres, lowrr_vsorres)
  rr_low <- pmin(highrr_vsorres, lowrr_vsorres)

  # - Calculate APS respiratory rate points
  rr_points <- dplyr::case_when(
    rr_high >= 50 | rr_low < 6  ~ 4,
    rr_high >= 35               ~ 3,
    rr_low < 10                 ~ 2,
    rr_high >= 25 | rr_low < 12 ~ 1,
    rr_high >= 12 | rr_low < 25 ~ 0,
  )

  # 5. Oxygenation: A-aDO2 if FiO2 >= 0.5, else PaO2
  # TODO: check other fio2 variables, use denominator of calc_sfratio_8a_0 (check calc_resp_support_type_0)
  # If ECMO, give 4 points
  fio2 <- get_value_with_lookback(daily_imv_fio2_8a_0, daily_imv_fio2_8a_m1, daily_imv_fio2_8a_m2)
  pao2 <- get_value_with_lookback(daily_pa02_lowest_0, daily_pa02_lowest_m1, daily_pa02_lowest_m2)
  paco2 <- get_value_with_lookback(daily_paco2_lowest_0, daily_paco2_lowest_m1, daily_paco2_lowest_m2)

  # Calculate A-aDO2 if FiO2 >= 0.5 (need PaCO2 for this)
  # A-aDO2 = (FiO2 * (760 - 47)) - (PaCO2 / 0.8) - PaO2
  # QUESTION: confirm formula? YES
  aado2 <- ifelse(!is.na(fio2) & fio2 >= 0.5 & !is.na(paco2) & !is.na(pao2),
                  (fio2 * (760 - 47)) - (paco2 / 0.8) - pao2,
                  NA)



  oxy_points <- dplyr::case_when(
    # A-aDO2 used if FiO2 >= 0.5
    # QUESTION: what to do if fio2 is missing? (particularly for checking fio2 >= 0.5)
    # - ASSUME < 0.5 (should be handled by above formula fix checking resp support type)
    !is.na(aado2) & aado2 >= 500 ~ 4,
    !is.na(aado2) & aado2 >= 350 ~ 3,
    !is.na(aado2) & aado2 >= 200 ~ 2,
    !is.na(aado2) & aado2 < 200 ~ 0,
    # PaO2 used if FiO2 < 0.5 or A-aDO2 not calculable
    !is.na(pao2) & pao2 > 70 ~ 0,
    !is.na(pao2) & pao2 >= 61 ~ 1,
    !is.na(pao2) & pao2 >= 55 ~ 3,
    !is.na(pao2) & pao2 < 55 ~ 4
  )

  # 6. Arterial pH
  ph <- get_value_with_lookback(daily_ph_lowest_0, daily_ph_lowest_m1, daily_ph_lowest_m2)
  ph_points <- dplyr::case_when(
    ph >= 7.7 ~ 4,
    ph >= 7.6 ~ 3,
    ph >= 7.5 ~ 1,
    ph >= 7.33 ~ 0,
    ph >= 7.25 ~ 2,
    ph >= 7.15 ~ 3,
    ph < 7.15 ~ 4
  )

  # 7. Serum Sodium (mmol/L)
  sodium <- get_value_with_lookback(daily_na_8a_0, daily_na_8a_m1, daily_na_8a_m2)
  sodium_points <- dplyr::case_when(
    sodium >= 180 ~ 4,
    sodium >= 160 ~ 3,
    sodium >= 155 ~ 2,
    sodium >= 150 ~ 1,
    sodium >= 130 ~ 0,
    sodium >= 120 ~ 2,
    sodium >= 111 ~ 3,
    sodium < 111  ~ 4
  )

  # 8. Serum Potassium (mmol/L)
  potassium <- get_value_with_lookback(daily_k_8a_0, daily_k_8a_m1, daily_k_8a_m2)
  k_points <- dplyr::case_when(
    potassium >= 7 ~ 4,
    potassium >= 6 ~ 3,
    potassium >= 5.5 ~ 1,
    potassium >= 3.5 ~ 0,
    potassium >= 3 ~ 1,
    potassium >= 2.5 ~ 2,
    potassium < 2.5 ~ 4
  )

  # 9. Serum Creatinine (mg/dL)
  # QUESTION: what about missing or "unknown" for `sofa_base_renal_dysnfx`?
  creatinine <- get_value_with_lookback(daily_cr_8a_0, daily_cr_8a_m1, daily_cr_8a_m2)

  cr_points <- dplyr::case_when(

    # Check whether or not to double points
    creatinine >= 3.5 & (
      sofa_base_renal_dysnfx != "Yes" |
      (
        sofa_base_renal_dysnfx == "Yes" &
        sofa_base_renal_chronic %in% c(
          "Creatinine 1.2 - 1.9 mg/dL",
          "Creatinine 2.0 - 3.4 mg/dL"
        )
      )
    ) ~ 8,
    creatinine >= 3.5 ~ 4,

    # Check whether or not to double points
    creatinine >= 2 & (
      sofa_base_renal_dysnfx != "Yes" |
      (sofa_base_renal_dysnfx == "Yes" & sofa_base_renal_chronic == "Creatinine 1.2 - 1.9 mg/dL")
    ) ~ 6,
    creatinine >= 2 ~ 3,

    # # Check whether or not to double points
    creatinine >= 1.5 & sofa_base_renal_dysnfx != "Yes" ~ 4,
    creatinine >= 1.5 ~ 2,

    # Don't double these two
    creatinine >= 0.6 ~ 0,
    creatinine < 0.6 ~ 2
  )

  # 10. Hematocrit (%)
  hematocrit <- get_value_with_lookback(daily_hct_8a_0, daily_hct_8a_m1, daily_hct_8a_m2)
  hct_points <- dplyr::case_when(
    hematocrit >= 60 ~ 4,
    hematocrit >= 50 ~ 2,
    hematocrit >= 46 ~ 1,
    hematocrit >= 30 ~ 0,
    hematocrit >= 20 ~ 2,
    hematocrit < 20 ~ 4
  )

  # 11. White Blood Cell Count (1000/mm³)
  wbc <- get_value_with_lookback(daily_wbc_8a_0, daily_wbc_8a_m1, daily_wbc_8a_m2)
  wbc_points <- dplyr::case_when(
    wbc >= 40 ~ 4,
    wbc >= 20 ~ 2,
    wbc >= 15 ~ 1,
    wbc >= 3 ~ 0,
    wbc >= 1 ~ 2,
    wbc < 1 ~ 4
  )

  # 12. Glasgow Coma Score (use 15 - GCS for points)
  gcs <- get_value_with_lookback(daily_gcs_8a_0, daily_gcs_8a_m1, daily_gcs_8a_m2)

  # - Convert to character from factor
  gcs <- as.character(gcs)
  # - Treat 'not documented' as NA
  gcs <- ifelse(gcs == 'not documented', NA, gcs)
  # - Extract quantitative component of GCS
  gcs <- as.numeric(sub('T', '', gcs))
  # - Calculate points
  gcs_points <- dplyr::case_when(
    !is.na(gcs) ~ 15 - gcs
  )

  # Sum all points
  total_aps <- rowSums(
    cbind(temp_points, map_points, hr_points, rr_points, oxy_points,
          ph_points, sodium_points, k_points, cr_points, hct_points,
          wbc_points, gcs_points),
    na.rm = FALSE  # Return NA if any component is missing
  )

  return(total_aps)
}


## -----------------------------------------------------------------------------
## Baseline Performance Status (Systematic DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable for frailty status on Day 0
#'
#' `calc_sys_frailty_status_0` calculates the first variable component of
#' the systematic DAG variable for baseline performance status from the data.
#'
#' @param frailty_base_code Integer vector. The `frailty_base_code` code map from the
#' `frailty_base` column from the data.
#' @param frailty_perf Character vector. The `frailty_perf` column from the data.
#'
#' @returns A vector with values:
#' - 0 = Not performed
#' - 1 = Very Fit
#' - 2 = Fit
#' - 3 = Managing Well
#' - 4 = Living with very mild frailty
#' - 5 = Living with mild frailty
#' - 6 = Living with moderate frailty
#' - 7 = Living with severe frailty
#' - 8 = Living with very severe frailty
#' - 9 = Terminally ill
#' - 99 = Unknown
#' @export
calc_sys_frailty_status_0 <- function(
  frailty_base_code,
  frailty_perf
) {
  dplyr::case_when(
    frailty_perf == "No" ~ 0,
    frailty_perf == "Yes" & !is.na(frailty_base_code) ~ as.numeric(frailty_base_code),
    frailty_perf == "Yes" & is.na(frailty_base_code) ~ 99
  )
}
