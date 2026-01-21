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



#' Calculate the systematic DAG variable for comorbidity burden on Day 0
#'
#' `calc_sys_comorbid_burden_0` calculates the weighted van Walraven
#' Elixhauser comorbidity score. This is a modification of the Elixhauser
#' comorbidity system into a point system for hospital mortality.
#'
#' Reference: van Walraven et al. Medical Care 2009;47(6):626-633
#' https://journals.lww.com/lww-medicalcare/fulltext/2009/06000/a_modification_of_the_elixhauser_comorbidity.4.aspx
#'
#' Van Walraven weights (from Table 1):
#' - Congestive heart failure: +7
#' - Cardiac arrhythmias: +5
#' - Valvular disease: -1
#' - Pulmonary circulation disorders: +4
#' - Peripheral vascular disorders: +2
#' - Hypertension: 0                        # Not calculated
#' - Paralysis: +7
#' - Neurodegenerative disorders: +6
#' - Chronic pulmonary disease: +3
#' - Diabetes (uncomplicated): 0            # Not calculated
#' - Diabetes (complicated): 0              # Not calculated
#' - Hypothyroidism: 0                      # Not calculated
#' - Renal failure: +5
#' - Liver disease: +11
#' - Peptic ulcer disease (no bleeding): 0  # Not calculated
#' - AIDS/HIV: 0                            # Not calculated
#' - Lymphoma: +9
#' - Metastatic cancer: +12
#' - Solid tumor without metastasis: +4
#' - Rheumatoid arthritis/collagen vascular diseases: 0 # Not calculated
#' - Coagulopathy: +3
#' - Obesity: -4
#' - Weight loss: +6
#' - Fluid and electrolyte disorders: +5
#' - Blood loss anemia: -2
#' - Deficiency anemia: -2
#' - Alcohol abuse: 0                       # Not calculated
#' - Drug abuse: -7
#' - Psychosis: 0                           # Not calculated
#' - Depression: -3
#'
#' @param m_cv_conditions___2 Checkbox. Heart failure (CHF = +7)
#' @param m_cv_conditions___5 Checkbox. Atrial fibrillation/flutter (cardiac arrhythmias = +5)
#' @param m_cv_conditions___6 Checkbox. Peripheral vascular disease (+2)
#' @param m_neurologic_conditions___4 Checkbox. Hemiplegia (paralysis = +7)
#' @param m_neurologic_conditions___5 Checkbox. Paraplegia (paralysis = +7)
#' @param m_neurologic_conditions___1 Checkbox. Dementia (neurodegenerative disorders = +6)
#' @param m_pulm_conditions___3 Checkbox. COPD (chronic pulmonary disease = +3)
#' @param m_kid_liver_conditions___1 Checkbox. Chronic kidney disease (renal failure = +5)
#' @param m_kid_liver_conditions___2 Checkbox. Cirrhosis (liver disease = +11)
#' @param m_cancer_conditions___2 Checkbox. Lymphoma (+9)
#' @param m_cancer_conditions___3 Checkbox. Multiple myeloma (metastatic cancer = +12)
#' @param m_cancer_conditions___4 Checkbox. Solid organ cancer (+4)
#' @param m_psych_conditions___1 Checkbox. Depression (-3)
#' @param susubcat___1 Checkbox. Cocaine use (drug abuse = -7)
#' @param susubcat___2 Checkbox. Oral opioids (drug abuse = -7)
#' @param susubcat___3 Checkbox. Injection opioids (drug abuse = -7)
#' @param susubcat___4 Checkbox. Methamphetamine (drug abuse = -7)
#' @param susubcat___5 Checkbox. Oral stimulants (drug abuse = -7)
#' @param susubcat___6 Checkbox. Oral sedatives (drug abuse = -7)
#' @param susubcat___7 Checkbox. Marijuana/Cannabis (drug abuse = -7)
#' @param susubcat___8 Checkbox. Hallucinogens (drug abuse = -7)
#' @param susubcat___9 Checkbox. Inhalants (drug abuse = -7)
#' @param susubcat___88 Checkbox. Other substance use (drug abuse = -7)
#' @param susubcat___99 Checkbox. Unknown substance use (drug abuse = -7)
#'
#' @returns Numeric vector with the weighted Elixhauser score (van Walraven method).
#'   Missing checkbox values treated as 0 (condition not present).
#' @export
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

  # Congestive heart failure (CHF): +7
  score <- score + dplyr::if_else(is_checked(m_cv_conditions___2), 7, 0)

  # Cardiac arrhythmias: +5 (atrial fib/flutter)
  score <- score + dplyr::if_else(is_checked(m_cv_conditions___5), 5, 0)

  # Valvular disease: -1
  # QUESTION: What variable captures this?

  # Pulmonary circulation disorders: +4
  # QUESTION: What variable(s) captures this?

  # Peripheral vascular disease: +2
  score <- score + dplyr::if_else(is_checked(m_cv_conditions___6), 2, 0)

  # Paralysis: +7 (hemiplegia or paraplegia)
  score <- score + dplyr::if_else(
    is_checked(m_neurologic_conditions___4) | is_checked(m_neurologic_conditions___5),
    7,
    0
  )

  # Neurodegenerative disorders: +6 (neuromuscular disorder or dementia)
  # QUESTION: Do we need to check more conditions?
  # - Examples: "1, Dementia (any type, including Alzheimer's Dementia) | 2, Prior stroke (cerebrovascular accident, CVA) | 3, Prior transient ischemic attack (TIA) | 4, Hemiplegia | 5, Paraplegia | 6, Neuromuscular disorder (such as myasthenia gravis, amyotrophic lateral sclerosis) | 7, Visual impairment, defined as not having the visual acuity to be able to read despite visual aids | 8, Hearing impairment, defined as not being able to hear normal conversation despite hearing aids | 9, Degenerative disc disease | 88, Other: {mh_neurologic_other:icons}"
  score <- score + dplyr::if_else(is_checked(m_neurologic_conditions___1), 6, 0)

  # Chronic pulmonary disease: +3 (COPD)
  # QUESTION: Do we need to check more conditions?
  # - Examples: "1, Asthma | 2, Bronchiectasis | 3, Chronic obstructive pulmonary disease (COPD) | 4, Home oxygen use | 5, Pulmonary hypertension | 6, Restrictive or interstitial lung disease | 88, Other: {mh_pulm_other:icons}"
  # - If whole list, just check if any (m_pulmonary == "Yes")
  score <- score + dplyr::if_else(is_checked(m_pulm_conditions___3), 3, 0)

  # Renal failure: +5 (chronic kidney disease)
  score <- score + dplyr::if_else(is_checked(m_kid_liver_conditions___1), 5, 0)

  # Liver disease: +11 (cirrhosis)
  score <- score + dplyr::if_else(is_checked(m_kid_liver_conditions___2), 11, 0)

  # Lymphoma: +9 (lymphoma only)
  # QUESTION: What about leukemia?
  score <- score + dplyr::if_else(is_checked(m_cancer_conditions___2), 9, 0)

  # Metastatic cancer: +12 (multiple myeloma)
  score <- score + dplyr::if_else(is_checked(m_cancer_conditions___3), 12, 0)

  # Solid tumor without metastasis: +4
  score <- score + dplyr::if_else(is_checked(m_cancer_conditions___4), 4, 0)

  # Coagulopathy: +3
  # QUESTION: What variable captures this?

  # Obesity: -4
  # QUESTION: What variable captures this?

  # Weight loss: +6
  # QUESTION: What variable captures this?

  # Fluid and electrolyte disorders: +5
  # QUESTION: What variable captures this?

  # Blood loss anemia: -2
  # QUESTION: What variable captures this?

  # Deficiency anemia: -2
  # QUESTION: What variable captures this?

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
