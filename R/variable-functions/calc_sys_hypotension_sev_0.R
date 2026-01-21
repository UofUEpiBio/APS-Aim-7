## -----------------------------------------------------------------------------
## Hypotension Severity (Systematic DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable for hypotension severity on Day 0
#'
#' `calc_sys_hypotension_sev_0` calculates the cardiovascular SOFA component
#' based on mean arterial pressure and vasopressor/inotrope use at 8am on Day 0.
#'
#' Based on SOFA cardiovascular criteria:
#' - 0 = No pressors or inotrope, MAP ≥70
#' - 1 = No pressors or inotrope, MAP <70
#' - 2 = (Dobutamine OR milrinone) AND (no dopamine OR dopamine ≤5) AND (no other vasopressors)
#' - 3 = On vasopressors with NEE ≤0.1
#' - 4 = NEE >0.1
#'
#' Citation for NEE calculation: Goradia S, Sardaneh AA, Narayan SW, Penm J,
#' Patanwala AE. Vasopressor dose equivalence: A scoping review and suggested
#' formula. J Crit Care, 2021;61:233–40.
#'
#' @param daily_vasopressors_0___0 Character vector. Checkbox for "no vasopressors".
#' @param daily_vasopressors_0___1 Character vector. Checkbox for "norepinephrine".
#' @param daily_vasopressors_0___2 Character vector. Checkbox for "epinephrine".
#' @param daily_vasopressors_0___3 Character vector. Checkbox for "phenylephrine".
#' @param daily_vasopressors_0___4 Character vector. Checkbox for "vasopressin".
#' @param daily_vasopressors_0___5 Character vector. Checkbox for "dopamine".
#' @param daily_vasopressors_0___6 Character vector. Checkbox for "dobutamine".
#' @param daily_vasopressors_0___7 Character vector. Checkbox for "angiotensin II".
#' @param daily_vasopressors_0___8 Character vector. Checkbox for "milrinone".
#' @param daily_vasopressors_0___88 Character vector. Checkbox for "other vasopressor".
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
#' - 0 = No pressors or inotrope, MAP ≥70
#' - 1 = No pressors or inotrope, MAP <70
#' - 2 = On dobutamine/milrinone only (with low/no dopamine)
#' - 3 = On vasopressors with NEE ≤0.1
#' - 4 = NEE >0.1
#' @export
calc_sys_hypotension_sev_0 <- function(
  daily_vasopressors_0___0,
  daily_vasopressors_0___1,
  daily_vasopressors_0___2,
  daily_vasopressors_0___3,
  daily_vasopressors_0___4,
  daily_vasopressors_0___5,
  daily_vasopressors_0___6,
  daily_vasopressors_0___7,
  daily_vasopressors_0___8,
  daily_vasopressors_0___88,

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

  ## Calculate MAP from SBP and DBP: MAP = (2 * DBP + SBP) / 3
  map <- (2 * daily_dbp_8a_0 + daily_sbp_8a_0) / 3

  ## Calculate norepinephrine equivalents (for vasopressors only)
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

  ## Calculate dopamine dose in mcg/min/kg for threshold check
  dopa_dose_mcgkg <- dplyr::coalesce(daily_dopa_dos_8a_0_mcgkg, daily_dopa_dose_8a_0_mcg / m_weight_kg, 0)

  ## Calculate dobutamine and milrinone doses
  dobuta_dose <- dplyr::coalesce(daily_dobuta_8a_0_mcg, daily_dobuta_8a_0_mcgkg * m_weight_kg, 0)
  milr_dose <- dplyr::coalesce(daily_milr_8a_0_mcg, daily_milr_8a_0_mcgkg * m_weight_kg, 0)

  ## Check if on any vasopressors (NE, epi, phenylephrine, vasopressin, angiotensin II)
  on_vasopressors <- is_checked(daily_vasopressors_0___1) | # norepinephrine
                        is_checked(daily_vasopressors_0___2) | # epinephrine
                        is_checked(daily_vasopressors_0___3) | # phenylephrine
                        is_checked(daily_vasopressors_0___4) | # vasopressin
                        is_checked(daily_vasopressors_0___5) | # dopamine
                        is_checked(daily_vasopressors_0___6) | # dobutamine
                        is_checked(daily_vasopressors_0___7) | # angiotensin II
                        is_checked(daily_vasopressors_0___8) | # milrinone
                        is_checked(daily_vasopressors_0___88)  # Other

  ## Check if on OTHER vasopressors (excludes dopamine, dobutamine, milrinone)
  on_other_vasopressors <- is_checked(daily_vasopressors_0___1) | # norepinephrine
                           is_checked(daily_vasopressors_0___2) | # epinephrine
                           is_checked(daily_vasopressors_0___3) | # phenylephrine
                           is_checked(daily_vasopressors_0___4) | # vasopressin
                           is_checked(daily_vasopressors_0___7) | # angiotensin II
                           is_checked(daily_vasopressors_0___88)  # Other

  dplyr::case_when(
    # Score 0: No pressors/inotropes, MAP ≥ 70
    is_checked(daily_vasopressors_0___0) & (map >= 70) ~ 0,

    # Score 1: No pressors/inotropes, MAP < 70
    is_checked(daily_vasopressors_0___0) & (map < 70) ~ 1,

    # Score 2: (dobutamine/milrinone) AND (no or <= 5 dopamine) AND (no other vasopressors)
    (is_checked(daily_vasopressors_0___6) | is_checked(daily_vasopressors_0___8)) &
        (dopa_dose_mcgkg <= 5) & !on_other_vasopressors ~ 2,

    # Score 3: On vasopressors with NEE ≤ 0.1
    on_vasopressors & (norepi_equiv <= 0.1) ~ 3,

    # Score 4: NEE > 0.1
    norepi_equiv > 0.1 ~ 4,
  )
}
