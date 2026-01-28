## -----------------------------------------------------------------------------
## Septic Shock (Systematic DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Septic Shock'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
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

  # ANSWERED: Is is appropriate to use OR is_checked(vasopressors) here? (alterative creates many more NA records)
  # - ANSWER: If final answer (after query) is that 'Unknown' won't be changed, then set those to 0.
  dplyr::case_when(
    (sepsis_present == "No") |
      (sepsis_clinical_judgement == "No") |
      is_checked(daily_vasopressors_0___0) |
      (norepi_equiv <= 0.5) ~ 0,

    (sepsis_present == "Yes") &
      (sepsis_clinical_judgement == "Yes") &
      is_unchecked(daily_vasopressors_0___0) &
      (norepi_equiv > 0.5) ~ 1,
    is_unknown(sepsis_clinical_judgement) ~ 99
  )
}
