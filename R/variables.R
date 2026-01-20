source("R/variable-functions/calc_sys_delirium_0.R")
source("R/variable-functions/calc_sys_sepsis_0.R")
source("R/variable-functions/calc_sys_ards_0.R")
source("R/variable-functions/calc_sys_pneumonia_0.R")
source("R/variable-functions/calc_sys_septic_shock_0.R")
source("R/variable-functions/calc_sys_organ_failure_trajectory.R")
source("R/variable-functions/calc_str_major_surgery_0.R")
source("R/variable-functions/calc_str_dementia_0.R")
source("R/variable-functions/calc_str_acute_allergic_reaction_0.R")
source("R/variable-functions/calc_str_dah_0.R")
source("R/variable-functions/calc_str_drug_toxicity_0.R")
source("R/variable-functions/calc_str_cop_0.R")
source("R/variable-functions/calc_str_scap_rr_0.R")
source("R/variable-functions/calc_str_scap_bun_0.R")
source("R/variable-functions/calc_str_scap_acidosis_0.R")
source("R/variable-functions/calc_str_scap_bilateral_opacities_0.R")
source("R/variable-functions/calc_str_scap_leukopenia_0.R")
source("R/variable-functions/calc_str_scap_thrombocytopenia_0.R")
source("R/variable-functions/calc_str_scap_hypothermia_0.R")
source("R/variable-functions/calc_str_neuromuscular_disease_0.R")
source("R/variable-functions/calc_str_vasculitis_0.R")
source("R/variable-functions/calc_str_septic_shock_0.R")
source("R/variable-functions/calc_str_gi_bleeding_0.R")
source("R/variable-functions/calc_str_psi_score_0.R")

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
