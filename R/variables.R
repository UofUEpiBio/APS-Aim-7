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
  daily_alb_nc_0, # branching logic
  daily_alb_8a_0,
  daily_tbili_nc_0, # branching logic
  daily_tbili_8a_0,
  daily_hct_nc_0, # branching logic
  daily_hct_8a_0,
  daily_wbc_nc_0, # branching logic
  daily_wbc_8a_0,
  daily_gluc_nc_0, # branching logic
  daily_gluc_8a_0,
  daily_bun_nc_0, # branching logic
  daily_bun_8a_0,
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
  daily_alb_nc_m1, # branching logic
  daily_alb_8a_m1,
  daily_tbili_nc_m1, # branching logic
  daily_tbili_8a_m1,
  daily_hct_nc_m1, # branching logic
  daily_hct_8a_m1,
  daily_wbc_nc_m1, # branching logic
  daily_wbc_8a_m1,
  daily_gluc_nc_m1, # branching logic
  daily_gluc_8a_m1,
  daily_bun_nc_m1, # branching logic
  daily_bun_8a_m1,
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
  daily_alb_nc_m2, # branching logic
  daily_alb_8a_m2,
  daily_tbili_nc_m2, # branching logic
  daily_tbili_8a_m2,
  daily_hct_nc_m2, # branching logic
  daily_hct_8a_m2,
  daily_wbc_nc_m2, # branching logic
  daily_wbc_8a_m2,
  daily_gluc_nc_m2, # branching logic
  daily_gluc_8a_m2,
  daily_bun_nc_m2, # branching logic
  daily_bun_8a_m2,
  daily_cr_nc_m2, # branching logic
  daily_cr_8a_m2,
  daily_gcs_8a_m2
) {
  dplyr::case_when(

  )
}

## -----------------------------------------------------------------------------
## Active SARS-CoV-2 Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable for active COVID-19 infection on Day 0
#'
#' `calc_sys_active_covid19_0` calculates the systematic DAG variable for
#' active COVID-19 infection from the data.
#'
#' @param has_pos_covid_0 Numeric vector. Indicator (1) if patient had a positive
#' COVID-19 test on or before enrollment, NA otherwise.
#'
#' @returns A vector with values:
#' - 0 = No active COVID-19 (not positive on or before Day 0, or not tested)
#' - 1 = Active COVID-19 (tested positive on or before Day 0)
#' @export
calc_sys_active_covid19_0 <- function(has_pos_covid_0) {
  dplyr::case_when(
    has_pos_covid_0 == 1 ~ 1,
    is.na(has_pos_covid_0) ~ 0
  )
}


## -----------------------------------------------------------------------------
## Active Influenza Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable for active influenza infection on Day 0
#'
#' `calc_sys_active_influenza_0` calculates the systematic DAG variable for
#' active influenza infection from the data.
#'
#' @param has_pos_influenza_0 Numeric vector. Indicator (1) if patient had a positive
#' influenza test on or before enrollment, NA otherwise.
#'
#' @returns A vector with values:
#' - 0 = No active influenza (not positive on or before Day 0, or not tested)
#' - 1 = Active influenza (tested positive on or before Day 0)
#' @export
calc_sys_active_influenza_0 <- function(has_pos_influenza_0) {
  dplyr::case_when(
    has_pos_influenza_0 == 1 ~ 1,
    is.na(has_pos_influenza_0) ~ 0
  )
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
