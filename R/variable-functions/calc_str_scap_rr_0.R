## -----------------------------------------------------------------------------
## SCAP Criterion: Respiratory Rate (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for SCAP respiratory rate criterion
#'
#' `calc_str_scap_rr_0` calculates the SCAP (Severe Community-Acquired Pneumonia)
#' respiratory rate criterion using Day 0 value with lookback to Days -1 and -2.
#'
#' - Update the DAG sheet
#' @param daily_resp_8a_0_code Integer vector. Respiratory support type code on Day 0.
#' @param daily_resp_8a_m1_code Integer vector. Respiratory support type code on Day -1.
#' @param daily_resp_8a_m2_code Integer vector. Respiratory support type code on Day -2.
#' @param daily_hfnc_rr_8a_0 Numeric vector. RR for HFNC on Day 0.
#' @param daily_hfnc_rr_8a_m1 Numeric vector. RR for HFNC on Day -1.
#' @param daily_hfnc_rr_8a_m2 Numeric vector. RR for HFNC on Day -2.
#' @param daily_niv_rr_8a_0 Numeric vector. RR for NIV on Day 0.
#' @param daily_niv_rr_8a_m1 Numeric vector. RR for NIV on Day -1.
#' @param daily_niv_rr_8a_m2 Numeric vector. RR for NIV on Day -2.
#' @param daily_standard_rr_8a_0 Numeric vector. RR for standard oxygen on Day 0.
#' @param daily_standard_rr_8a_m1 Numeric vector. RR for standard oxygen on Day -1.
#' @param daily_standard_rr_8a_m2 Numeric vector. RR for standard oxygen on Day -2.
#' @param daily_resp_rate_8a_0 Numeric vector. RR for IMV on Day 0.
#' @param daily_resp_rate_8a_m1 Numeric vector. RR for IMV on Day -1.
#' @param daily_resp_rate_8a_m2 Numeric vector. RR for IMV on Day -2.
#' @param highrr_vsorres Numeric vector. Highest RR for other support types.
#'
#' @returns A numeric vector with values:
#' - 0 = Respiratory rate ≤ 30
#' - 1 = Respiratory rate > 30
#' @export
calc_str_scap_rr_0 <- function(
  daily_resp_8a_0_code,
  daily_resp_8a_m1_code,
  daily_resp_8a_m2_code,

  daily_hfnc_rr_8a_0,
  daily_hfnc_rr_8a_m1,
  daily_hfnc_rr_8a_m2,

  daily_niv_rr_8a_0,
  daily_niv_rr_8a_m1,
  daily_niv_rr_8a_m2,

  daily_standard_rr_8a_0,
  daily_standard_rr_8a_m1,
  daily_standard_rr_8a_m2,

  daily_resp_rate_8a_0,
  daily_resp_rate_8a_m1,
  daily_resp_rate_8a_m2,

  highrr_vsorres
  ) {

  ## Helper function to get RR based on respiratory support type
  get_rr_for_support <- function(resp_code, imv_rr, niv_rr, hfnc_rr, standard_rr, high_rr) {
    dplyr::case_when(
      resp_code == 1 ~ imv_rr,       # ECMO and IMV
      resp_code == 2 ~ high_rr,      # ECMO without IMV
      resp_code == 3 ~ imv_rr,       # IMV without ECMO
      resp_code == 4 ~ niv_rr,       # Non-invasive ventilation
      resp_code == 5 ~ hfnc_rr,      # High-flow nasal oxygen (HFNC)
      resp_code == 6 ~ standard_rr,  # Standard flow supplemental oxygen
      resp_code == 7 ~ high_rr,      # No respiratory support (room air)
      resp_code == 99 ~ high_rr,     # Unknown
      TRUE ~ NA_real_
    )
  }

  ## Get RR for each day
  rr_0 <- get_rr_for_support(
    daily_resp_8a_0_code,
    daily_resp_rate_8a_0,
    daily_niv_rr_8a_0,
    daily_hfnc_rr_8a_0,
    daily_standard_rr_8a_0,
    highrr_vsorres
  )

  rr_m1 <- get_rr_for_support(
    daily_resp_8a_m1_code,
    daily_resp_rate_8a_m1,
    daily_niv_rr_8a_m1,
    daily_hfnc_rr_8a_m1,
    daily_standard_rr_8a_m1,
    highrr_vsorres
  )

  rr_m2 <- get_rr_for_support(
    daily_resp_8a_m2_code,
    daily_resp_rate_8a_m2,
    daily_niv_rr_8a_m2,
    daily_hfnc_rr_8a_m2,
    daily_standard_rr_8a_m2,
    highrr_vsorres
  )

  ## Get final RR with lookback (Day 0 -> Day -1 -> Day -2)
  rr <- get_value_with_lookback(rr_0, rr_m1, rr_m2)

  ## Apply SCAP criterion
  dplyr::case_when(
    rr <= 30 ~ 0,
    rr > 30 ~ 1
  )
}
