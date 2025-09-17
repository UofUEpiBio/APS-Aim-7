#' Derive variable `neuromuscular_blockade_0`
#'
#' `derive_neuromuscular_blockade_0` derives the neuromuscular blockade
#' variable from the data. This variable represents daily neuromuscular blocking
#' agent use on admission day.
#'
#' Input data must contain the columns:
#' - `daily_paralysis_0`
#'
#' @param data Dataframe.
#'
#' @returns A vector with values:
#' - 0 = No neuromuscular blockade on day 0 or missing (NA)
#' - 1 = Neuromuscular blockade given on day 0
#' @export
derive_neuromuscular_blockade_0 <- function(data) {

  required_vars <- c(
    "daily_paralysis_0"
  )

  validate_required_variables(
    data = data,
    required_vars = required_vars,
    function_name = "derive_neuromuscular_blockade_0"
  )

  nmb <- as.character(data$daily_paralysis_0)

  nmb <- ifelse(nmb == 'Administered', 1, 0)
  nmb[is.na(nmb)] <- 0

  as.numeric(nmb)
}

#' Derive streamlined inflammatory profile variable
#'
#' `derive_streamlined_inflammatory_profile_0` derives the inflammatory
#' profile variable from the data. Using the streamlined formula. This
#' variable represents hyperinflammatory profile closest to 8am on this
#' day.
#'
#' Input data must contain the columns:
#' - `daily_crp_8a_0`
#'
#' @inheritParams derive_neuromuscular_blockade_0
#'
#' @returns A vector with values:
#' - 0 = Day 0 CRP not checked or < 15 or missing (NA)
#' - 1 = Day 0 CRP checked and ≥ 15
#' @export
derive_streamlined_inflammatory_profile_0 <- function(data) {

  required_vars <- c(
    "daily_crp_8a_0"
  )

  validate_required_variables(
    data = data,
    required_vars = required_vars,
    function_name = "derive_streamlined_inflammatory_profile_0"
  )

  ifp <- data$daily_crp_8a_0

  ifp <- ifelse(ifp >= 15, 1, 0)
  ifp[is.na(ifp)] <- 0

  ifp
}

#' Derive systematic respiratory failure severity variables
#'
#' `derive_systematic_respiratory_failure_severity_0` derives the
#' respiratory failure severity variable from the data using the
#' systematic formula. This variable is calculated using the P:F
#' ratio (or S:F) ratio, PEEP, and daily maximal respiratory
#' support.
#'
#' Input data must contain the columns:
#' - `daily_standard_flow_8a_0`
#' - `daily_hfnc_fi02_8a_0`
#' - `daily_niv_fi02_8a_0`
#' - `daily_imv_fio2_8a_0`
#' - `daily_spo2_8a_0`
#' - `daily_epap_8a_0`
#' - `daily_o2_lowest_0`
#' - `daily_fio2_lowest_0`
#' - `daily_resp_lowest_0`
#'
#' @inheritParams derive_neuromuscular_blockade_0
#'
#' @returns A named list with two vectors:
#' - `rfs_var1`: A vector with values:
#'   - 0 = No respiratory support on day 0 or missing (NA)
#'   - 1 = Standard flow
#'   - 2 = HFNC
#'   - 3 = NIV
#'   - 4 = IMV with PEEP < 12
#'   - 5 = IMV with PEEP ≥ 12
#' - `rfs_var2`: A vector with the S:F or P:F ratio on day 0 based on `rfs_var1`:
#'   - 0 = [8a SpO2]/[0.21)
#'   - 1 = [8a SpO2]/[0.21 + (0.03 * 8a O2 flow)
#'   - 2, 3, 4, 5 = [8a SpO2]/[8a FiO2]
#' @export
derive_systematic_respiratory_failure_severity_0 <- function(data) {

  required_vars <- c(
    "daily_standard_flow_8a_0",
    "daily_hfnc_fi02_8a_0",
    "daily_niv_fi02_8a_0",
    "daily_imv_fio2_8a_0",
    "daily_spo2_8a_0",
    "daily_epap_8a_0",
    "daily_fio2_lowest_0"
  )

  validate_required_variables(
    data = data,
    required_vars = required_vars,
    function_name = "derive_systematic_respiratory_failure_severity_0"
  )

  # Compute Systematic Variable #1
  rfs_var1 <- ifelse(!is.na(data$daily_standard_flow_8a_0), 1,
      ifelse(!is.na(data$daily_hfnc_fi02_8a_0), 2,
      ifelse(!is.na(data$daily_niv_fi02_8a_0), 3,
      ifelse(!is.na(data$daily_imv_fio2_8a_0) & data$daily_epap_8a_0 < 12, 4,
      ifelse(!is.na(data$daily_imv_fio2_8a_0) & data$daily_epap_8a_0 >= 12, 5,
      0)))))
  # rfs_var1[is.na(rfs_var1)] <- 0

  # Compute Systematic Variable #2
  rfs_var2 <- ifelse(rfs_var1 == 0, data$daily_spo2_8a_0 / 0.21,
      ifelse(rfs_var1 == 1, data$daily_spo2_8a_0 / (0.21 + (0.03 * data$daily_standard_flow_8a_0)),
      data$daily_spo2_8a_0 / data$daily_fio2_lowest_0
      ))

  return(list(rfs_var1 = rfs_var1, rfs_var2 = rfs_var2))
}

#' Helper function to identify "Checked" values
is_checked <- function(x) {
  x == "Checked"
}

#' Helper function to identify "Unchecked" values
is_unchecked <- function(x) {
  x == "Unchecked"
}

compute_NEE <- function(ne_dose, epi_dose, phen_dose) {
  ne_dose[is.na(ne_dose)] <- 0
  epi_dose[is.na(epi_dose)] <- 0
  phen_dose[is.na(phen_dose)] <- 0

  nee <- ne_dose + epi_dose + phen_dose

  nee
}

#' Derive hypotension severity variable
#' 
#' `derive_hypotension_severity_0` derives the hypotension severity variable
#' from the data. This variable represents the severity of hypotension on
#' admission day.
#'
#' Input data must contain the columns:
#' - `daily_vasopressors_0___0` (None)
#' - `daily_vasopressors_0___1` (Norepinephrine)
#' - `daily_vasopressors_0___2` (Epinephrine)
#' - `daily_vasopressors_0___3` (Phenylephrine)
#' - `daily_vasopressors_0___4` (Vasopressin)
#' - `daily_vasopressors_0___5` (Dopamine)
#' - `daily_vasopressors_0___6` (Dobutamine)
#' - `daily_vasopressors_0___7` (Angiotensin II)
#' - `daily_vasopressors_0___8` (Milrinone)
#' - `daily_vasopressors_0___88` (Other)
#' - `daily_sysbp_8a_0`
#' - `daily_dbp_8a_0`
#' - `daily_ne_dose_8a_0_mcgkg`
#' - `daily_epi_dose_8a_0_mcgkg`
#' - `daily_phen_dos_8a_0_mcgkg`
#' - `daily_dopa_dos_8a_0_mcgkg`
#' 
#' @inheritParams derive_neuromuscular_blockade_0
#' 
#' @returns A vector with values:
#' - 0 = No pressors or inotrope, MAP >= 70
#' - 1 = No pressors or inotrope, MAP < 70
#' - 2 = (dobutamine OR milrinone) AND (no dopamine or dopamine <= 5) AND (no other vasopressors)
#' - 3 = On vasopressors with NEE < 0.1
#' - 4 = NEE > 0.1 AND NEE < 0.25
#' - 5 = NEE >= .25
#' @export
derive_hypotension_severity_0 <- function(data) {

  required_vars <- c(
    "daily_vasopressors_0___0", # None
    "daily_vasopressors_0___1", # Norepinephrine
    "daily_vasopressors_0___2", # Epinephrine
    "daily_vasopressors_0___3", # Phenylephrine
    "daily_vasopressors_0___4", # Vasopressin
    "daily_vasopressors_0___5", # Dopamine
    "daily_vasopressors_0___6", # Dobutamine
    "daily_vasopressors_0___7", # Angiotensin II
    "daily_vasopressors_0___8", # Milrinone
    "daily_vasopressors_0___88", # Other
    "daily_sbp_8a_0",
    "daily_dbp_8a_0",
    "daily_ne_dose_8a_0_mcgkg",
    "daily_epi_dose_8a_0_mcgkg",
    "daily_phen_dos_8a_0_mcgkg",
    "daily_dopa_dos_8a_0_mcgkg"
  )

  validate_required_variables(
    data = data,
    required_vars = required_vars,
    function_name = "derive_hypotension_severity_0"
  )

  MAP_score <- (2 * data$daily_dbp_8a_0 + data$daily_sbp_8a_0) / 3
  NEE_score <- compute_NEE(
    ne_dose = data$daily_ne_dose_8a_0_mcgkg,
    epi_dose = data$daily_epi_dose_8a_0_mcgkg,
    phen_dose = data$daily_phen_dos_8a_0_mcgkg
  )

  hs <- ifelse(
    # No pressors or inotrope, MAP >= 70
    (is_checked(data$daily_vasopressors_0___0)) & (MAP_score >= 70), 0,
    # No pressors or inotrope, MAP < 70
    ifelse((is_checked(data$daily_vasopressors_0___0)), 1,
    # (dobutamine OR milrinone) AND (no dopamine or dopamine <= 5) AND (no other vasopressors)
    ifelse(
      (is_checked(data$daily_vasopressors_0___6) | is_checked(data$daily_vasopressors_0___8)) & 
      (is.na(data$daily_vasopressors_0___5) | data$daily_dopa_dos_8a_0_mcgkg <= 5) & 
      (
        is_unchecked(data$daily_vasopressors_0___7) & 
        is_unchecked(data$daily_vasopressors_0___4) & 
        is_unchecked(data$daily_vasopressors_0___3) & 
        is_unchecked(data$daily_vasopressors_0___2) & 
        is_unchecked(data$daily_vasopressors_0___1) & 
        is_unchecked(data$daily_vasopressors_0___88)
      ), 2,
    # On vasopressors with NEE < 0.1
    ifelse((is_unchecked(data$daily_vasopressors_0___0)) & (NEE_score <= 0.1), 3,
    # NEE > 0.1 AND NEE < 0.25
    ifelse((NEE_score > 0.1) & (NEE_score < 0.25), 4,
    # NEE >= .25
    5
  )))))

  hs
}