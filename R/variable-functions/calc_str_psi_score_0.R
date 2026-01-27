## -----------------------------------------------------------------------------
## PSI Score (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'PSI Score'
#
# Value: Pneumonia Severity Index (PSI) score
calc_str_psi_score_0 <- function(
  age,
  sex,
  prssrc,

  m_cv_conditions___2,
  m_neurologic_conditions___2,
  m_neurologic_conditions___3,
  m_cancer,
  m_kid_liver_conditions___1,
  m_kid_liver_conditions___2,

  cam_0,
  cam_m1,
  cam_m2,

  highhr_vsorres,
  highrr_vsorres,
  lowsysbp_vsorres,
  lowtemp_vsorres,
  hightemp_vsorres,

  daily_bun_8a_0,
  daily_bun_8a_m1,
  daily_bun_8a_m2,

  daily_gluc_8a_0,
  daily_gluc_8a_m1,
  daily_gluc_8a_m2,

  daily_hct_8a_0,
  daily_hct_8a_m1,
  daily_hct_8a_m2,

  daily_na_8a_0,
  daily_na_8a_m1,
  daily_na_8a_m2,

  daily_pa02_lowest_0,
  daily_pa02_lowest_m1,
  daily_pa02_lowest_m2,

  daily_ph_lowest_0,
  daily_ph_lowest_m1,
  daily_ph_lowest_m2,

  pna_effusion
) {

  # Demographics

  # - Age
  score <- age

  # - Sex: Female -10 points
  score <- score + dplyr::if_else(sex == "Female", -10, 0)


  # Nursing home resident: +10 points
  score <- score + dplyr::if_else(
    prssrc == 'Nursing home or Skilled Nursing Facility (SNF)', 10,
    0,
    missing = 0
  )

  # QUESTION: DAG sheet includes these variables, but PSI does not use them:
  # - `su_ivdu`: recreational injection drug use?
  # - `sualcdosfrq`/`sualcdstxt`/`sualcdosfrqsix`: alcohol use frequency

  # Coexisting Illnesses
  # - Neoplastic disease: +30 points
  score <- score + dplyr::if_else(m_cancer == "Yes", 30, 0, missing = 0)

  # - Liver disease: +20 points (Cirrhosis)
  score <- score + dplyr::if_else(is_checked(m_kid_liver_conditions___2), 20, 0, missing = 0)

  # - Congestive heart failure: +10 points
  score <- score + dplyr::if_else(is_checked(m_cv_conditions___2), 10, 0, missing = 0)

  # - Cerebrovascular disease: +10 points (Prior stroke or TIA)
  score <- score + dplyr::if_else(
    is_checked(m_neurologic_conditions___2) | is_checked(m_neurologic_conditions___3), 10,
    0,
    missing = 0
  )

  # - Renal disease: +10 points (Chronic kidney disease)
  score <- score + dplyr::if_else(is_checked(m_kid_liver_conditions___1), 10, 0, missing = 0)

  # Physical examination findings

  # - Altered mental status: +20 points
  # QUESTION: Do we need to look at anything other than delirium CAM?
  cam <- get_value_with_lookback(cam_0, cam_m1, cam_m2)
  score <- score + dplyr::if_else(
    cam == "Positive for delirium at least once on this day", 20,
    0,
    missing = 0
  )

  # - Respiratory rate >= 30/min: +20 points
  # QUESTION: How close to the SCAP RR calculation should this be?
  # - That formula performs aggressive lookback based on respiratory support type,
  # - but scores on <=30 and >30, so we can't use it directly.
  # - Currently using highest RR on Day 0, until decided otherwise.
  score <- score + dplyr::if_else(highrr_vsorres >= 30, 20, 0, missing = 0)

  # - SBP < 90 mm Hg: +20 points
  score <- score + dplyr::if_else(lowsysbp_vsorres < 90, 20, 0, missing = 0)

  # - Temperature < 35°C or >= 40°C: +15 points
  # QUESTION: Should we check both lowtemp_vsorres and hightemp_vsorres for each value?
  temp_abnormal <- (lowtemp_vsorres < 35) | (lowtemp_vsorres >= 40) |
                   (hightemp_vsorres < 35) | (hightemp_vsorres >= 40)
  score <- score + dplyr::if_else(temp_abnormal, 15, 0, missing = 0)

  # - Pulse >= 125/min: +10 points
  score <- score + dplyr::if_else(highhr_vsorres >= 125, 10, 0, missing = 0)


  # Laboratory and radiographic findings

  # - Arterial pH < 7.35: +30 points
  ph_lowest <- get_value_with_lookback(daily_ph_lowest_0, daily_ph_lowest_m1, daily_ph_lowest_m2)
  score <- score + dplyr::if_else(ph_lowest < 7.35, 30, 0, missing = 0)

  # - BUN >= 30 mg/dl: +20 points
  bun <- get_value_with_lookback(daily_bun_8a_0, daily_bun_8a_m1, daily_bun_8a_m2)
  score <- score + dplyr::if_else(bun >= 30, 20, 0, missing = 0)

  # - Sodium < 130 mmol/L: +20 points
  na <- get_value_with_lookback(daily_na_8a_0, daily_na_8a_m1, daily_na_8a_m2)
  score <- score + dplyr::if_else(na < 130, 20, 0, missing = 0)

  # - Glucose >= 250 mg/dl: +10 points
  gluc <- get_value_with_lookback(daily_gluc_8a_0, daily_gluc_8a_m1, daily_gluc_8a_m2)
  score <- score + dplyr::if_else(gluc >= 250, 10, 0, missing = 0)

  # - Hematocrit < 30%: +10 points
  hct <- get_value_with_lookback(daily_hct_8a_0, daily_hct_8a_m1, daily_hct_8a_m2)
  score <- score + dplyr::if_else(hct < 30, 10, 0, missing = 0)

  # - PaO2 < 60 mm Hg: +10 points
  pa02_lowest <- get_value_with_lookback(daily_pa02_lowest_0, daily_pa02_lowest_m1, daily_pa02_lowest_m2)
  score <- score + dplyr::if_else(pa02_lowest < 60, 10, 0, missing = 0)

  # - Pleural effusion: +10 points (pna_effusion = Yes)
  score <- score + dplyr::if_else(pna_effusion == "Yes", 10, 0, missing = 0)

  return(score)
}
