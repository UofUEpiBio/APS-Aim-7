## -----------------------------------------------------------------------------
## Septic Shock (Systematic DAG)
## -----------------------------------------------------------------------------

## HELPER FUNCTION: Calculate norepinephrine equivalents
## Conversion factors for vasopressor doses
# TODO: Check/update this calculation after completing the hypotension severity NEE calculation
calc_norepi_equivalents <- function(
  daily_ne_dose_8a_0_mcg,
  daily_ne_dose_8a_0_mcgkg,
  daily_epi_dose_8a_0_mcg,
  daily_epi_dose_8a_0_mcgkg,
  daily_phen_dose_8a_0_mcg,
  daily_phen_dos_8a_0_mcgkg,
  daily_vaso_dose_8a_0,
  daily_dopa_dose_8a_0_mcg,
  daily_dopa_dos_8a_0_mcgkg,
  daily_ang2_8a_0_mcg,
  daily_ang2_8a_0_mcgkg,
  m_weight_kg
) {

  ## Convert all doses to mcg/min using weight if needed
  ne_dose <- dplyr::coalesce(daily_ne_dose_8a_0_mcg, daily_ne_dose_8a_0_mcgkg * m_weight_kg, 0)
  epi_dose <- dplyr::coalesce(daily_epi_dose_8a_0_mcg, daily_epi_dose_8a_0_mcgkg * m_weight_kg, 0)
  phen_dose <- dplyr::coalesce(daily_phen_dose_8a_0_mcg, daily_phen_dos_8a_0_mcgkg * m_weight_kg, 0)
  dopa_dose <- dplyr::coalesce(daily_dopa_dose_8a_0_mcg, daily_dopa_dos_8a_0_mcgkg * m_weight_kg, 0)
  ang2_dose <- dplyr::coalesce(daily_ang2_8a_0_mcg, daily_ang2_8a_0_mcgkg * m_weight_kg, 0)
  vaso_dose <- dplyr::coalesce(daily_vaso_dose_8a_0, 0)

  ## Calculate norepinephrine equivalents
  ## NE = 1, Epi = 1, Phenylephrine = 10, Vasopressin = 0.04 units = 1 mcg NE equivalent
  ## Dopamine at high doses ~ 0.01, Angiotensin II ~ 1
  norepi_equiv <- ne_dose +
                  epi_dose +
                  (phen_dose / 10) +
                  (vaso_dose / 0.04) +
                  (dopa_dose * 0.01) +
                  ang2_dose

  return(norepi_equiv)
}


#' Calculate the systematic DAG variable for severe septic shock on Day 0
#'
#' `calc_sys_septic_shock_0` calculates the systematic DAG variable for
#' severe septic shock, defined as sepsis with vasopressors > 0.5 norepinephrine
#' equivalents at 8am on Day 0.
#'
#' @param sepsis_present Character vector. The `sepsis_present` column from the data.
#' @param sepsis_clinical_judgement Character vector. The `sepsis_clinical_judgement` column.
#' @param daily_vasopressors_0___0 Character vector. Checkbox for "no vasopressors". Checked = no vasopressors, Unchecked = on vasopressors.
#' @param daily_sbp_8a_0 Numeric vector. Systolic BP at 8am on Day 0.
#' @param daily_dbp_8a_0 Numeric vector. Diastolic BP at 8am on Day 0.
#' @param daily_ne_dose_8a_0_mcg Numeric vector. Norepinephrine dose (mcg/min).
#' @param daily_ne_dose_8a_0_mcgkg Numeric vector. Norepinephrine dose (mcg/min/kg).
#' @param daily_epi_dose_8a_0_mcg Numeric vector. Epinephrine dose (mcg/min).
#' @param daily_epi_dose_8a_0_mcgkg Numeric vector. Epinephrine dose (mcg/min/kg).
#' @param daily_phen_dose_8a_0_mcg Numeric vector. Phenylephrine dose (mcg/min).
#' @param daily_phen_dos_8a_0_mcgkg Numeric vector. Phenylephrine dose (mcg/min/kg).
#' @param daily_vaso_dose_8a_0 Numeric vector. Vasopressin dose (units/min).
#' @param daily_dopa_dose_8a_0_mcg Numeric vector. Dopamine dose (mcg/min).
#' @param daily_dopa_dos_8a_0_mcgkg Numeric vector. Dopamine dose (mcg/min/kg).
#' @param daily_dobuta_8a_0_mcg Numeric vector. Dobutamine dose (mcg/min).
#' @param daily_dobuta_8a_0_mcgkg Numeric vector. Dobutamine dose (mcg/min/kg).
#' @param daily_ang2_8a_0_mcg Numeric vector. Angiotensin II dose (mcg/min).
#' @param daily_ang2_8a_0_mcgkg Numeric vector. Angiotensin II dose (mcg/min/kg).
#' @param daily_milr_8a_0_mcg Numeric vector. Milrinone dose (mcg/min).
#' @param daily_milr_8a_0_mcgkg Numeric vector. Milrinone dose (mcg/min/kg).
#' @param m_weight_kg Numeric vector. Patient weight in kg.
#'
#' @returns A numeric vector with values:
#' - 0 = Severe septic shock not present
#' - 1 = Severe septic shock present
#' - 99 = Unknown
#' @export
calc_sys_septic_shock_0 <- function(
  sepsis_present,
  sepsis_clinical_judgement,
  daily_vasopressors_0___0,
  daily_sbp_8a_0,
  daily_dbp_8a_0,
  daily_ne_dose_8a_0_mcg,
  daily_ne_dose_8a_0_mcgkg,
  daily_epi_dose_8a_0_mcg,
  daily_epi_dose_8a_0_mcgkg,
  daily_phen_dose_8a_0_mcg,
  daily_phen_dos_8a_0_mcgkg,
  daily_vaso_dose_8a_0,
  daily_dopa_dose_8a_0_mcg,
  daily_dopa_dos_8a_0_mcgkg,
  daily_dobuta_8a_0_mcg,
  daily_dobuta_8a_0_mcgkg,
  daily_ang2_8a_0_mcg,
  daily_ang2_8a_0_mcgkg,
  daily_milr_8a_0_mcg,
  daily_milr_8a_0_mcgkg,
  m_weight_kg
) {

  ## Calculate norepinephrine equivalents
  norepi_equiv <- calc_norepi_equivalents(
    daily_ne_dose_8a_0_mcg = daily_ne_dose_8a_0_mcg,
    daily_ne_dose_8a_0_mcgkg = daily_ne_dose_8a_0_mcgkg,
    daily_epi_dose_8a_0_mcg = daily_epi_dose_8a_0_mcg,
    daily_epi_dose_8a_0_mcgkg = daily_epi_dose_8a_0_mcgkg,
    daily_phen_dose_8a_0_mcg = daily_phen_dose_8a_0_mcg,
    daily_phen_dos_8a_0_mcgkg = daily_phen_dos_8a_0_mcgkg,
    daily_vaso_dose_8a_0 = daily_vaso_dose_8a_0,
    daily_dopa_dose_8a_0_mcg = daily_dopa_dose_8a_0_mcg,
    daily_dopa_dos_8a_0_mcgkg = daily_dopa_dos_8a_0_mcgkg,
    daily_ang2_8a_0_mcg = daily_ang2_8a_0_mcg,
    daily_ang2_8a_0_mcgkg = daily_ang2_8a_0_mcgkg,
    m_weight_kg = m_weight_kg
  )

  # QUESTION: Is is appropriate to use OR is_checked(vasopressors) here? (alterative creates many more NA records)
  dplyr::case_when(
    (sepsis_present == "No") |
      (sepsis_clinical_judgement == "No") |
      is_checked(daily_vasopressors_0___0) |
      (norepi_equiv <= 0.5) ~ 0,

    (sepsis_present == "Yes") &
      (sepsis_clinical_judgement == "Yes") &
      is_unchecked(daily_vasopressors_0___0) &
      (norepi_equiv > 0.5) ~ 1,
    sepsis_clinical_judgement == "Unknown" ~ 99
  )
}
