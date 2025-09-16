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
#' - 0 = No neuromuscular blockade on day 0 or missing
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

#' Derive variable `inflammatory_profile_0`
#'
#' `derive_inflammatory_profile_0` derives the inflammatory profile variable
#' from the data. This variable represents hyperinflammatory profile — CRP,
#' ferritin, fibrinogen, D-dimer closest to 8am on this day. Currently, only
#' uses the streamlined formula, not systematic (will be added later).
#' 
#' Input data must contain the columns:
#' - `daily_crp_8a_0`
#'
#' @inheritParams derive_neuromuscular_blockade_0
#'
#' @returns A vector with values:
#' - 0 = Day 0 CRP not checked or < 15
#' - 1 = Day 0 CRP checked and ≥ 15
#' @export
derive_inflammatory_profile_0 <- function(data) {

  required_vars <- c(
    "daily_crp_8a_0"
  )

  validate_required_variables(
    data = data,
    required_vars = required_vars,
    function_name = "derive_inflammatory_profile_0"
  )

  ifp <- data$daily_crp_8a_0

  ifp <- ifelse(ifp >= 15, 1, 0)
  ifp[is.na(ifp)] <- 0

  ifp
}

#' Derive variable `respiratory_failure_severity_0`
#' 
#' `respiratory_failure_severity_0` derives the respiratory failure severity
#' variable from the data. This variable is calculated using the P:F ratio
#' (or S:F) ratio, PEEP, and daily maximal respiratory support. Curerntly, only
#' uses the systematic formula, not streamlined (will be added later).
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
#' @returns A vector with values:
#' - 0 = Day 0 CRP not checked or < 15
#' - 1 = Day 0 CRP checked and ≥ 15
#' @export
derive_respiratory_failure_severity_0 <- function(data) {

  required_vars <- c(
    "daily_standard_flow_8a_0",
    "daily_hfnc_fi02_8a_0",
    "daily_niv_fi02_8a_0",
    "daily_imv_fio2_8a_0", # TODO: Check this, listed in document as daily_imv_fio2_8a_1
    "daily_spo2_8a_0",
    "daily_epap_8a_0",
    "daily_o2_lowest_0", # Start of "streamlined" list
    "daily_fio2_lowest_0",
    "daily_resp_lowest_0"
  )

  validate_required_variables(
    data = data,
    required_vars = required_vars,
    function_name = "derive_respiratory_failure_severity_0"
  )

  # Compute Systematic Variable #1
  rfs_var1 <- ifelse(!is.na(data$daily_standard_flow_8a_0), 1,
      ifelse(!is.na(data$daily_hfnc_fi02_8a_0), 2,
      ifelse(!is.na(data$daily_niv_fi02_8a_0), 3,
      ifelse(!is.na(data$daily_imv_fio2_8a_0) & data$daily_epap_8a_0 < 12, 4,
      ifelse(!is.na(data$daily_imv_fio2_8a_0) & data$daily_epap_8a_0 >= 12, 5,
      0)))))
  rfs_var1[is.na(rfs_var1)] <- 0

  # Compute Systematic Variable #2
  rfs_var2 <- ifelse(rfs_var1 == 0, data$daily_spo2_8a_0 / 0.21,
      ifelse(rfs_var1 == 1, data$daily_spo2_8a_0 / (0.21 + (0.03 * data$daily_standard_flow_8a_0)),
      data$daily_spo2_8a_0 / data$daily_fio2_lowest_0
      ))

  return(list(rfs_var1 = rfs_var1, rfs_var2 = rfs_var2))
}
