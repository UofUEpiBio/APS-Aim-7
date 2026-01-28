## -----------------------------------------------------------------------------
## Baseline Performance Status (Systematic DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Baseline Performance Status: Frailty Status'
#
# Values:
# - 0 = Not performed
# - 1 = Very Fit
# - 2 = Fit
# - 3 = Managing Well
# - 4 = Living with very mild frailty
# - 5 = Living with mild frailty
# - 6 = Living with moderate frailty
# - 7 = Living with severe frailty
# - 8 = Living with very severe frailty
# - 9 = Terminally ill
# - 99 = Unknown
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



# Calculate SYSTEMATIC DAG 'Baseline Performance Status: Comorbidity Burden'
#
# Value: Weighted van Walraven Elixhauser comorbidity score
calc_sys_comorbid_burden_0 <- function(
  # Cardiovascular conditions
  m_cv_conditions___2,      # CHF
  m_cv_conditions___5,      # Atrial fib/flutter
  m_cv_conditions___6,      # Peripheral vascular disease

  # Neurologic conditions
  m_neurologic_conditions___4,  # Hemiplegia
  m_neurologic_conditions___5,  # Paraplegia
  m_neurologic_conditions___1,  # Dementia

  # Pulmonary conditions
  m_pulm_conditions___3,    # COPD

  # Kidney and liver conditions
  m_kid_liver_conditions___1,  # Chronic kidney disease
  m_kid_liver_conditions___2,  # Cirrhosis

  # Cancer conditions
  m_cancer_conditions___2,  # Lymphoma
  m_cancer_conditions___3,  # Multiple myeloma
  m_cancer_conditions___4,  # Solid organ cancer

  # Psychiatric conditions
  m_psych_conditions___1,   # Depression

  # Substance use (drug abuse)
  susubcat___1,             # Cocaine
  susubcat___2,             # Oral opioids
  susubcat___3,             # Injection opioids
  susubcat___4,             # Methamphetamine
  susubcat___5,             # Oral stimulants
  susubcat___6,             # Oral sedatives
  susubcat___7,             # Marijuana/Cannabis
  susubcat___8,             # Hallucinogens
  susubcat___9,             # Inhalants
  susubcat___88,            # Other substance
  susubcat___99             # Unknown substance
) {

  # Initialize score at 0
  score <- 0

  # TODO: Update will the correct variables (listed under each variable name)
  # - Also switch to Charlson comorbidity index (will need to handle edge cases)

  # Congestive heart failure (CHF): +7
  # - m_cv_conditions___2
  score <- score + dplyr::if_else(is_checked(m_cv_conditions___2), 7, 0)

  # Cardiac arrhythmias: +5 (atrial fib/flutter)
  # - m_cv_conditions___5 (should be more, might be in other)
  score <- score + dplyr::if_else(is_checked(m_cv_conditions___5), 5, 0)

  # Valvular disease: -1
  # - Don't have (maybe look into it)

  # Pulmonary circulation disorders: +4
  # - m_pulm_conditions___5

  # Peripheral vascular disease: +2
  # - m_cv_conditions___6 OR m_cv_conditions___7
  score <- score + dplyr::if_else(is_checked(m_cv_conditions___6), 2, 0)

  # Paralysis: +7 (hemiplegia or paraplegia)
  # - m_neurologic_conditions___4 OR m_neurologic_conditions___5
  score <- score + dplyr::if_else(
    is_checked(m_neurologic_conditions___4) | is_checked(m_neurologic_conditions___5),
    7,
    0
  )

  # Neurodegenerative disorders: +6 (neuromuscular disorder or dementia)
  # - m_neurologic_conditions___1 OR m_neurologic_conditions___6
  score <- score + dplyr::if_else(is_checked(m_neurologic_conditions___1), 6, 0)

  # Chronic pulmonary disease: +3 (COPD)
  # - m_pulm_conditions___1 OR m_pulm_conditions___2 OR m_pulm_conditions___3 OR m_pulm_conditions___6
  score <- score + dplyr::if_else(is_checked(m_pulm_conditions___3), 3, 0)

  # Renal failure: +5 (chronic kidney disease)
  # - m_kid_liver_conditions___1
  score <- score + dplyr::if_else(is_checked(m_kid_liver_conditions___1), 5, 0)

  # Liver disease: +11 (cirrhosis)
  # - m_kid_liver_conditions___2
  score <- score + dplyr::if_else(is_checked(m_kid_liver_conditions___2), 11, 0)

  # Lymphoma: +9 (lymphoma only)
  # - m_cancer_conditions___2
  score <- score + dplyr::if_else(is_checked(m_cancer_conditions___2), 9, 0)

  # Metastatic cancer: +12 (multiple myeloma)
  # - m_cancer_conditions___4 AND mhsolidmeta == 'Yes'
  score <- score + dplyr::if_else(is_checked(m_cancer_conditions___3), 12, 0)

  # Solid tumor without metastasis: +4
  # - m_cancer_conditions___4 AND mhsolidmeta == 'No'/'Unknown'
  score <- score + dplyr::if_else(is_checked(m_cancer_conditions___4), 4, 0)

  # Coagulopathy: +3
  # - Don't have (maybe look into it)

  # Obesity: -4
  # - Calculate BMI (look up formula): m_weight_kg, m_height (in inches convert to m_height_cm)
  # - BMI >= 30.0 == YES for obesity

  # Weight loss: +6
  # - Don't have (probably won't)

  # Fluid and electrolyte disorders: +5
  # - Don't have (probably won't)

  # Blood loss anemia: -2
  # -

  # Deficiency anemia: -2
  # -

  # Drug abuse: -7 (any recreational drug use excluding tobacco/alcohol)
  score <- score + dplyr::if_else(
    is_checked(susubcat___1) |
    is_checked(susubcat___2) |
    is_checked(susubcat___3) |
    is_checked(susubcat___4) |
    is_checked(susubcat___5) |
    is_checked(susubcat___6) |
    is_checked(susubcat___7) |
    is_checked(susubcat___8) |
    is_checked(susubcat___9) |
    is_checked(susubcat___88) |
    is_checked(susubcat___99),
    -7,
    0
  )

  # Depression: -3
  score <- score + dplyr::if_else(is_checked(m_psych_conditions___1), -3, 0)

  return(score)
}


# Convenience wrapper function
# Returns a data frame with record_id, sys_frailty_status_0, and sys_comorbid_burden_0 columns (one row per record_id)
wrapper_calc_sys_baseline_performance_status_0 <- function(data, dictionary) {
  # Get frailty data from Patient/Surrogate Interview
  data_with_frailty <- data |>
    filter(event_label == "Patient/Surrogate Interview") |>
    left_join(
      get_code_label_map('frailty_base', dictionary),
      by = 'frailty_base'
    ) |>
    mutate(
      sys_frailty_status_0 = calc_sys_frailty_status_0(
        frailty_base_code = frailty_base_code,
        frailty_perf = frailty_perf
      )
    ) |>
    select(record_id, sys_frailty_status_0)

  # Get comorbidity data from Day 0 and Patient/Surrogate Interview
  data_with_comorbid <- data |>
    filter(event_label == 'Day 0') |>
    # Remove substance use variables and merge them back in from Patient/Surrogate Interview
    select(-c(susubcat___1, susubcat___2, susubcat___3, susubcat___4,
              susubcat___5, susubcat___6, susubcat___7, susubcat___8,
              susubcat___9, susubcat___88, susubcat___99)) |>
    # Join Patient/Surrogate Interview variables (substance use)
    left_join(data |>
      filter(event_label == 'Patient/Surrogate Interview') |>
      select(record_id, susubcat___1, susubcat___2, susubcat___3, susubcat___4,
             susubcat___5, susubcat___6, susubcat___7, susubcat___8,
             susubcat___9, susubcat___88, susubcat___99),
      by = 'record_id') |>
    mutate(
      sys_comorbid_burden_0 = calc_sys_comorbid_burden_0(
        # Cardiovascular conditions
        m_cv_conditions___2,
        m_cv_conditions___5,
        m_cv_conditions___6,
        # Neurologic conditions
        m_neurologic_conditions___4,
        m_neurologic_conditions___5,
        m_neurologic_conditions___1,
        # Pulmonary conditions
        m_pulm_conditions___3,
        # Kidney and liver conditions
        m_kid_liver_conditions___1,
        m_kid_liver_conditions___2,
        # Cancer conditions
        m_cancer_conditions___2,
        m_cancer_conditions___3,
        m_cancer_conditions___4,
        # Psychiatric conditions
        m_psych_conditions___1,
        # Substance use
        susubcat___1,
        susubcat___2,
        susubcat___3,
        susubcat___4,
        susubcat___5,
        susubcat___6,
        susubcat___7,
        susubcat___8,
        susubcat___9,
        susubcat___88,
        susubcat___99
      )
    ) |>
    select(record_id, sys_comorbid_burden_0)

  data |>
    distinct(record_id) |>
    left_join(data_with_frailty, by = 'record_id') |>
    left_join(data_with_comorbid, by = 'record_id')
}
